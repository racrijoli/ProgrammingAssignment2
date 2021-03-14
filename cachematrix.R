## Per Professor Peng's request, this function creates a matrix which can cache its inverse.

## This is the first function of the ProgrammingAssignment2.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = getInverse, setInverse = setInverse, getInverse = getInverse)
  
}


## This is the second function of the ProgrammingAssignment2, which computes the inverse of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
  
}
