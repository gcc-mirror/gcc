      subroutine saxpy(n,sa,sx,incx,sy,incy)
C
C     constant times a vector plus a vector.
C     uses unrolled loop for increments equal to one.
C     jack dongarra, linpack, 3/11/78.
C     modified 12/3/93, array(1) declarations changed to array(*)
C
      real sx(*),sy(*),sa
      integer i,incx,incy,ix,iy,m,mp1,n
C
C  -ffast-math ICE provoked by this conditional
      if(sa /= 0.0)then
C
C        code for both increments equal to 1
C
	      do i= 1,n
		sy(i)= sy(i)+sa*sx(i)
		enddo
	endif
      return
      end
