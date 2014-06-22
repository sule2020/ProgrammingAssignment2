##-------CREATING A "MATRIX" OBJECT THAT CAN CACHE ITS INVERSE---

makeCacheMatrix <- function(x = matrix()) {
# Setting initial value for the object
      matrix.inv <- NULL
      set <- function(y) {
      x <<- y
      matrix.inv <<- NULL
      }
# getting the value of the matrix.
      get <- function() x
# setting the inversed matrix.
      set.inv <- function(i) matrix.inv <<- i
      get.inv <- function() matrix.inv
# getting the inversed matrix.
      list(set = set, get = get,
           set.inv = set.inv,
           get.inv = get.inv)
}


# Calculating the inverse. If the inverse was calculated and not changed, then function
# cachesolve should retrieve the inverse from the cache to save time-consuming

cacheSolve <- function(x, ...) {
       
# getting the inverse of the matrix.
      matrx.inv <- x$get.inv()
# check if there is already the matrix then print the alert message.
      if(!is.null(matrx.inv)) {
      print("getting cached matdata")
      return(matrx.inv)
      }
  
# if not then get the inverse of the matrix.
      data <- x$get()
      matrx.inv <- solve(data, ...)
# set the inverse of the matrix.
      x$set.inv(matrx.inv)
      matrx.inv
}

