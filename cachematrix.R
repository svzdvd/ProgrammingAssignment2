## 
## This file contains two functions:
## - makeCacheMatrix() creates a matrix that can contain a cached inverse
## - cacheSolve() gets a cached matrix inverse or calculates it
##

## 
## Create a matrix that can contain a cached inverse
## 
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  # set a new matrix
  set <- function(y) {
    x <<- y
    # delete previously cached inverse
    cachedInverse <<- NULL
  }
  
  # get matrix
  get <- function() {
    x
  }
  
  # set cached inverse for current matrix
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  # get cached inverse
  getInverse <- function() {
    cachedInverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##
## Return a matrix that is the inverse of 'x':
## check if the inverse has already been calculated, if so, return the cached inverse.
## Otherwise calculate the inverse of 'x', put it in the cache and then return it.
##
cacheSolve <- function(x, ...) {
  # get cached inverse
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    # a cached inverse has been found
    message("getting cached data")
    return(inverse)
  }
  
  # cached inverse not found...
  
  # calculate inverse
  data <- x$get()
  inverse <- solve(data, ...)
  
  # put it in the cache
  x$setInverse(inverse)
  
  inverse
}