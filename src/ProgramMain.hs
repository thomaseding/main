module ProgramMain (
      main
    ) where


import Data.List
import Data.Monoid
import System.Environment


main :: IO ()
main = do
    [moduleName] <- getArgs >>= return . map (stripExt "hs")
    putStrLn $ mainModuleText moduleName >>= expandTab 4


type Extension = String


stripExt :: Extension -> FilePath -> FilePath
stripExt ext file = maybe file id $ stripSuffix ext' file
    where
	ext' = case ext of
	    '.' : _ -> ext
	    _ -> '.' : ext


stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse


expandTab :: Int -> Char -> String
expandTab n c = case c of
    '\t' -> replicate n ' '
    _ -> [c]


type ModuleName = String


mainModuleText :: ModuleName -> String
mainModuleText moduleName = unlines [
      "module Main ("
    , "\t  main"
    , "\t) where"
    , ""
    , ""
    , "import qualified " ++ moduleName
    , ""
    , ""
    , "main :: IO ()"
    , "main = " ++ moduleName ++ ".main"
    , ""
    ]




