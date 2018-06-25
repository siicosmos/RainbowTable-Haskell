import RainbowAssign
import Data.Int
import Data.List()
import Data.Maybe as Maybe
import qualified Data.Map as Map

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5           -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000            -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

------------------------------------------------------------------------------------------------
-- actuall reducce int function
reduceInt32 :: Int32 -> String
reduceInt32 i = foldl (++) "" [mapAndReverse(convert (fromIntegral(i)))]

-- map "toLetter" to list of int and reverse the order and also take only pwLength elements
mapAndReverse :: [Int] -> [Char]
mapAndReverse x = reverse(take pwLength (map toLetter x))

-- convert base 10 to base nLetters
convert :: Int -> [Int]
convert 0 = [0]
convert n
    | n > 0 = let(d,r) = n `divMod` nLetters in r : (convert d) ++ [0,0..]
    | n < 0 = let(d,r) = (n `div` nLetters, n - (n `div` nLetters)*nLetters) in r : convert d

-- password reduce function wrapper
pwReduce ::  Hash -> Passwd
pwReduce h = reduceInt32 h

------------------------------------------------------------------------------------------------
-- convert two list to list of tuples by combining each element in each list
tupleUp :: [a] -> [b] -> [(a,b)]
tupleUp xs [] = []
tupleUp [] ys = []
tupleUp (x:xs) (y:ys) = (x,y): tupleUp xs ys

-- generate nth column
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable n xs = Map.fromList(tupleUp (rainbowTableTail n xs) xs)
    where
        rainbowTableTail 0 xs = map pwHash xs
        rainbowTableTail n xs = rainbowTableTail (n-1) (map pwReduce (map pwHash xs))

------------------------------------------------------------------------------------------------
-- generate the full n columns full rainbow table of a initial password list
rainbowFullTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowFullTable n xs = Map.fromList(rainbowFullTableTail [] n)
    where
        rainbowFullTableTail l 0 = (rainbowTableNthTuple 0 xs) ++ l
        rainbowFullTableTail l n = rainbowFullTableTail (rainbowTableNthTuple n xs ++ l) (n-1)

rainbowTableNthTuple :: Int -> [Passwd] -> [(Hash,Passwd)]
rainbowTableNthTuple 0 xs = tupleUp (map pwHash xs) xs
rainbowTableNthTuple n xs = rainbowTableNthTuple (n-1) (map pwReduce (map pwHash xs))

-- reverse hash
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword t w h = Map.lookup h table
    where
        table = rainbowFullTable w (Map.elems t)

------------------------------------------------------------------------------------------------ tests
-- generate a new table and save it to disk
generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename

test1 :: IO (Maybe Passwd)
test1 = do
    table <- readTable filename
    return (Map.lookup 2131624990 table)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
    table <- readTable filename
    pws <- randomPasswords nLetters pwLength n
    let hs = map pwHash pws
    let result = Maybe.mapMaybe (findPassword table width) hs
    return (result, length result)

main :: IO ()
main = do
    generateTable
    res <- test2 10000
    print res