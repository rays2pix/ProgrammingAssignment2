## This script provides function to compute inverse of  a matrix. The inverse is computed only once 
## and cached. The later requests to access inverse are retrieved from the cache

## This function takes a matrix as input and creates a list of
## access methods for the matrix and its inverse. << operator is used
## access the object which is in parent environment

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function returns the inverse of the matrix. The function takes an object which is returned 
##by above function,computes inverse only when it is called for the first time and at later times
##just returns the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
