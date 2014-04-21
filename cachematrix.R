## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create a environment where it placed two variables M (matrix) and invM (matriz inversa)
##   and hold four method, two for set and get the matrix, and other two por set and get the inverse matrix
##   We declare an objet "makeCacheMatrix" with the sintax 
##   object1 <- makeCacheMatrix(M = matrix(c(1,0,0,1,nrow =2, ncol=2).
##   We get the matrix using object1$getMatrix and set using object1$setMatrix
##   Initially the inverse is not calculated. We use the cacheSolve for this purpose

makeCacheMatrix<- function(M = matrix()) {
  invM <- NULL
  setMatrix <- function(y) {
    M <<- y
    invM <<- NULL
  }
  getMatrix <- function() M
  setMInv <- function(Inv) invM <<- Inv
  getMInv <- function() invM
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setMInv = setMInv,
       getMInv = getMInv)
}

## cacheSolve is a fuction which take all the environment of an object declares with makeCacheMatrix and
##   calculate the inverse if it not calculated instead and it is possible. If places the result on the 
##   environment declared as object1


cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
  invCalc <- M$getMInv()
  if(!is.null(invCalc)) {
    message("getting cached data")
    return(invCalc)
  }
  matrixData <- M$getMatrix()
  if(!det(matrixData)){
    invCalc <- NaN
    
  }else{
    invCalc <- solve(matrixData, ...)
    M$setMInv(invCalc)
  }
  invCalc
}
