# Put comments here that give an overall description of what your
## functions do

## Matrix to store the Inverse

b <- matrix()
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())  
{
  m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(solve) m <<- b
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse )
}


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  ##--------------------------- OR this method -##
  # m<- resolve(data,...)
  #resolve <- function(data,...)
  #{b <- svd(data) 
  # c <- b$v%*%diag(1/b$d)%*%t(b$u)
  #return(c)}
  ##--------------------------------------------##
  
  x$setinverse(m)
  
  m
}
