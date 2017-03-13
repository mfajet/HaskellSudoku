import Data.List
import Data.List.Split
import Control.Parallel.Strategies
import Control.Parallel

check :: Int -> [Int] -> Bool
check num xs= sort xs == [1..num]

board::[[Int]]
board = [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6]]

board2 :: [[Int]]
board2 = [[8,3,5,4,1,6,9,2,7], [2,9,6,8,5,7,4,3,1], [4,1,7,2,9,3,6,5,8], [5,6,9,1,3,4,7,8,2], [1,2,3,6,7,8,5,4,9], [7,4,8,5,2,9,1,6,3], [6,5,2,7,8,1,3,9,4], [9,8,1,3,4,5,2,7,6], [3,7,4,9,6,2,8,1,5]]
checkRows :: [[Int]] -> Bool
checkRows board = and ((parMap rpar) (check 9) board)

checkColumns :: [[Int]] -> Bool
checkColumns board = checkRows (transpose board)

checkSquares :: [[Int]] -> Bool
checkSquares xs = checkRows $ checkHelper ((floor . sqrt . fromIntegral ) (length [1..9])) xs

checkHelper :: Int -> [[Int]] -> [[Int]]
checkHelper num xs = map concat $ concat $ map (chunksOf num) (transpose(map (chunksOf num) xs))

checkBoard board = and [checkRows board, checkColumns board, checkSquares board]

main = print $ checkBoard board2
