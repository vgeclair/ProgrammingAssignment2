## makeCacheMatrix creates a matrix, and cacheSolve calculates its inverse
## or get the cached one if it's already calculated. If the matrix is changed, 
## then the inverse matrix should be calculated again.

## Making a matrix, for example:
## m = rbind(c(1, -1/4), c(-1/4, 1))  
## m2 = makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## Calculating the inverse of a matrix or get the cached inverse.
## For example:
## cachesolve(m2) calculates the inverse matrix, and
## if you use it again, the cached matrix would be returned.
## However, if the matrix is changed using m2$set(new_matrix), 
## the inverse matrix needs to be calculated again

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
