## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# What this function does is to cache the inverse of a matrix that calculates 
# function cachesolve.  function cachesolve calculates it if the function is
# not cached in  function makeCacheMatrix and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i     ## Return a matrix that is the inverse of 'x'
}

a<-matrix(rnorm(4),2,2)
c<-matrix(c(1:4),2,2)
b<-makeCacheMatrix(c)
cacheSolve(b)
