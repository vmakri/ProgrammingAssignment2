## with makeCacheMatrix we exploit already computed values of inverse matrix stored in cached variable
## if allready computed is fetched without computation
 
## makeCacheMatrix creates a list of four functions, 1) to set initially global m to null
## and store matrix  to x 2) return matrix x 3) to store the inverse of x to global m
## and 4) return the inverse of x
 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
 
 
## cacheSolve first checks if the inverse of x is allready calculated
## and stored in m. If this is the case, fetches it.
## Else stores x in matrix, calculates the inverse through solve function
## and updates m with setmatrix function
 
 
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
