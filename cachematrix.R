## This function creates a special "matrix" objec that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##function computes the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
        
  k <- x$getinverse()
  if (!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  ## Return a matrix that is the inverse of x
  k
}
