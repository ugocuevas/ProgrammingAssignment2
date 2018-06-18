## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. 

## The following functions are to be used one after the other (not to be combined at once) to achieve the desired result 

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## This list caches the inverse of the matrix when it is calculated incase it is needed again


makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      set <- function(b) {
            
            x <<- b
            
            m <<- NULL
            
      }
      
      get <- function() x
      
      setInverse <- function(inverse) m <<- inverse
      
      getInverse <- function() m
      
      list(set = set, get = get,
           
           setInverse = setInverse,
           
           getInverse = getInverse)

}


## Write a short comment describing this function

## The second function cacheSolve takes the list containing the matrix and finds the inverse of the matrix
## If the inverse of the matrix has been calculated previously, this function draws the value from the list
## If the inverse of the matrix has not been calculated, this function calculates the value and stores it in the list

cacheSolve <- function(x, ...) {
      
      m <- x$getInverse()
      
      if(!is.null(m)) {
            
            message("getting cached data")
            
            return(m)
            
      }
      
      data <- x$get()
      
      m <- solve(data)
      
      x$setInverse(m)
      
      m
        ## Return a matrix that is the inverse of 'x'
}
