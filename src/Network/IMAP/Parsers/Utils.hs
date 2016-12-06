module Network.IMAP.Parsers.Utils where

import Network.IMAP.Types

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Either.Combinators (rightToMaybe)
import Control.Monad (liftM)

import Data.Set (Set)
import qualified Data.Set as Set

eatUntilClosingParen :: Parser BSC.ByteString
eatUntilClosingParen = scan 0 hadClosedAllParens <* char ')'

hadClosedAllParens :: Int -> Char -> Maybe Int
hadClosedAllParens openingParenCount char
  | char == ')' =
    if openingParenCount == 1
      then Nothing
      else Just $ openingParenCount - 1
  | char == '(' = Just $ openingParenCount + 1
  | otherwise =  Just openingParenCount


parseEmailList :: Parser [EmailAddress]
parseEmailList = char '(' *> parseEmail `sepBy` char ' ' <* char ')'

parseNString :: Parser T.Text
parseNString = do
  char '"'
  nstring <- AP.takeWhile1 (/= '"')
  char '"'
  return $ decodeUtf8 nstring

parseEmail :: Parser EmailAddress
parseEmail = do
  char '('
  label <- nilOrValue $ parseNString
  char ' '
  route <- nilOrValue $ parseNString
  char ' '

  emailUsername <- nilOrValue $ parseNString
  char ' '
  emailDomain <- nilOrValue $ parseNString
  char ' '

  return $ EmailAddress label route emailUsername emailDomain

nilOrValue :: Parser a -> Parser (Maybe a)
nilOrValue parser = rightToMaybe <$> AP.eitherP (string "NIL") parser

parseQuotedText :: Parser T.Text
parseQuotedText = do
  char '"'
  date <- AP.takeWhile1 (/= '"')
  char '"'

  return . decodeUtf8 $ date

parseNameAttribute :: Parser NameAttribute
parseNameAttribute = do
  string "\\"
  name <- AP.takeWhile1 isAtomChar
  return $ case name of
          "Noinferiors" -> Noinferiors
          "Noselect" -> Noselect
          "Marked" -> Marked
          "Unmarked" -> Unmarked
          "HasNoChildren" -> HasNoChildren
          _ -> OtherNameAttr $ decodeUtf8 name

parseListLikeResp :: BSC.ByteString -> Parser UntaggedResult
parseListLikeResp prefix = do
  string prefix
  string " ("
  nameAttributes <- parseNameAttribute `sepBy` char ' '

  string ") \""
  delimiter <- liftM (decodeUtf8 . BSC.singleton) AP.anyChar
  string "\" "
  name <- liftM decodeUtf8 $ AP.takeWhile1 (/= '\r')

  let actualName = T.dropAround (== '"') name
  return $ ListR nameAttributes delimiter actualName

atomSpecials :: Set Char
atomSpecials = Set.fromList "(){ %*\\\n\r]\0"

isAtomChar :: Char -> Bool
isAtomChar = flip Set.notMember atomSpecials

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]

parseNumber :: (Int -> a) -> BSC.ByteString ->
  BSC.ByteString -> Parser (Either ErrorMessage a)
parseNumber constructor prefix postfix = do
  if not . BSC.null $ prefix
    then string prefix <* char ' '
    else return BSC.empty
  number <- takeWhile1 isDigit
  if not . BSC.null $ postfix
    then char ' ' *> string postfix
    else return BSC.empty

  return $ liftM constructor (toInt number)
