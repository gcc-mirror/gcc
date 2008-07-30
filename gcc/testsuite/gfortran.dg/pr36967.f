! { dg-options "-O2 -fpredictive-commoning" }
      subroutine foo(x,y,n)
      integer n
      real*8 y(n,n,n),x(n,n,n)
      integer k, j, i
      do k = 2, n-1
        do j = 2, n-1
          do I = 2, n-1
            y(i,j,k) = y(i,j,k)
     +        + (x(i-1,j-1,k)
     +           +  x(i,j-1,k-1)
     +           +  x(i,j+1,k-1)
     +           +  x(i,j+1,k+1)
     +           +  x(i+1,j,k+1))
     +        + (x(i-1,j-1,k-1)
     +           +  x(i+1,j-1,k-1)
     +           +  x(i-1,j+1,k-1)
     +           +  x(i+1,j+1,k-1)
     +           +  x(i-1,j+1,k+1)
     +           +  x(i+1,j+1,k+1))
          enddo
        enddo
      enddo
      return
      end
