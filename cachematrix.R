## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve work together to calculate and store
## a matrix inverse and the matrix itself with the use of caching.

## Write a short comment describing this function
## This is the helper function for set and get of the data and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This is the inverse calculator for the matrix and returns either the cached (if exists)
## or calculated inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
