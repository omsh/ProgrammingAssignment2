## The first funciton creates a list of functions for the given matrix x; 
## namely, there are two functions as setter and getter for the matrix itself and two functions 
## as setter and getter for the inverse of the matrix.
## The second function solves for the inverse if there was no cached value (not previously computed)

## the function that  creates the cached data and returns a list of functions as described above

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


## The function that computes the inverse taking into consideration cached data (previously computed inverse)

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
