makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL  
  evn <- environment()  
  y<-NULL 
  
  setmatrix<-function(y){  
    x<<-y  
    m<<-NULL 
  }
  
  getmatrix<-function() x #function to get the matrix
  setinverse<-function(solve) m<<- solve  #function to find the inverse
  getinverse<-function() m  #finction to display the inverse
  getenv<- function() environment() #to display the result
  
  list (setmatrix=setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}



cacheSolve <- function(xMat= m(), ...) {
  
  m <- xMat$getinverse() 
  if(!is.null(m)){ 
    if(xMat$setmatrix() == xMat$getmatrix()) { 
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    
    y <- xMat$getmatrix() 
    xMat$setmatrix(y) 
    m <- solve(y, ...) 
    xMat$setinverse(m) 
    m 
  }
  
}
