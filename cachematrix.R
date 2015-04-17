## This file contains two functions makeCacheMatrix and cacheSolve 
## detailed below


## makeCacheMatrix sets up a list of functions that sets data matrix, 
## gets datamatrix, sets inverse of datamatrix, gets inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##  cacheSolve checks to see if the inverse has already been calculated, if so, 
##  it pulls from cache, else it calculates it and stores in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
