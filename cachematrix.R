## This script has two function. mackeCacheMatrix & cacheSolve which works only for Square matrix [n,n]
# Here are the instructions to run these functions:
# 1) Create a matrix e.g b<- diag(5,3)
# 2) Make another variable using the function e.g  cachedMatrix <- makeCacheMatrix(b)
# 3) call the cacheSolve function to create the inverse

## makeCacheMatrix creates  4 functions and 2 data objects
# Functions are: get, set, getInverse and setInverse
# DataObjects: x - matrix and i - inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## solve in an in-built function to return inverse of a square matrix
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve - returns inverse of an input Matrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
