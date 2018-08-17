# Assignment 2 program --------------------------------------------------------------

## makeCacheMatrix():

  ## This function computes and stores the inverse of a matrix, 
  ## The result (inverse of the matric) can later be retrived using cacheSolve()

## cacheSolve(): This is a two-step function

  ## When executed, it first determines whether the inverse of a matric has been 
  ## computed and stored using makeCacheMatrix()

    ## If the inverse of matrix has been previously computed:
    ## It retrieves the result 
  
    ## if there's no stored result from makeCacheMatrix():
    ## It calculates the inverse of the matrix 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  m <-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setsolve <-function(solve) m <<- solve
  getsolve <-function() m
  list(set = set, get = get, setsolve = setsolve, getsolve =getsolve)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <-x$getsolve()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}


# Test ------------------------------------------------------------------------------

test_mat = matrix(
  c(4,2,7,6),
  nrow =2,
  ncol =2
)

test_mat_2 = matrix (
  c(5,7,8,9,9),
  nrow =5,
  ncol =5
)
a <-makeCacheMatrix(test_mat)
a$get()
a$getsolve()

cacheSolve(a)
cacheSolve(a)

