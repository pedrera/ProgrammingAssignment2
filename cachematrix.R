## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates one matrix object with the methods:
# get matrix
# set matrixParam to matrix 
# get matrix inverse
# set inverseParam to matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  get <- function() x
  set <- function(matrixParam) {
    x <<- matrixParam
    inverseMatrix <<- NULL
  }
  getInverse <- function() inverseMatrix
  setInverse <- function(inverseParam) {
    inverseMatrix <<- inverseParam
  }
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


# cacheSolve calculates the inverse of a matrix.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  ## Inverse Matrix Calculation only If inverseMatrix is null 
  if(is.null(inverseMatrix)) {
    inverseMatrix <- solve(x$get())
    x$setInverse(inverseMatrix)
  }
  inverseMatrix
}
