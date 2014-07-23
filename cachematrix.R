## cachematrix
## a pair of functions that cache the inverse of a matrix.

## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here).

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## Builds a makeCacheMatrix 'object' with 4 functions
makeCacheMatrix <- function(x = matrix()) {
  # Invalidate our cache data
  m <- NULL
  
  # Sets a new value of x (a matrix)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Returns the value of x (a matrix)
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # Solve for the inverse
  m <- solve(data, ...)
  x$setinverse(m)
  m
}