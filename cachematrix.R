## Function that saves the inverse matrix from one matrix until it change.
## If the matrix changes then save new matrix and new inverse amtrix

## Thi function save the inverse matrix from the last inverse process.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(minv) inv <<- minv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function take the makeCahceMatrix and store the inverse, in the matrix is the same return the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  mat <- x$getinv()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinv(mat)
  mat
}
