
## MakeCacheMatrix creates a special "matrix" object, 
## that is list of functions to following:
##    1. Set the value of the matrix
##    2. Get the Value of the matrix
##    3. Set the value of inverse of the matrix
##    4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)

}


## cacheSolve function calculates the inverse of the "matrix" created 
## using makeCacheMatrix. It first checks if the inverse of matrix is 
## already available in cache. If the cached inverse is available, then it returns 
## the cached inverse data. Otherwise, it calcluates the inverse of the matrix, 
## caches the inverse and then returns the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}


