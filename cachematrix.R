makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) s <<- inverse
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  # if the inverse matrix has already been calculated then write messagge:getting cached solution
  if(!is.null(s)) {
    message("getting cached solution")
    return(s)
  }
  # otherwise calculates the inverse matrix with solve function
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  return(s)
}

#Test
c<-makeCacheMatrix(m)
cacheSolve(c)
