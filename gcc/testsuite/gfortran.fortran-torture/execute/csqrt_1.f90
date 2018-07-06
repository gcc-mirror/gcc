! PR 14396
! These we failing on targets which do not provide the c99 complex math
! functions.
! Extracted from intrinsic77.f in the g77 testsuite.
      logical fail
      common /flags/ fail
      fail = .false.
      call square_root
      if (fail) STOP 1
      end
      subroutine square_root
      intrinsic sqrt, dsqrt, csqrt
      real x, a
      x = 4.0
      a = 2.0
      call c_r(SQRT(x),a,'SQRT(real)')
      call c_d(SQRT(1.d0*x),1.d0*a,'SQRT(double)')
      call c_c(SQRT((1.,0.)*x),(1.,0.)*a,'SQRT(complex)')
      call c_d(DSQRT(1.d0*x),1.d0*a,'DSQRT(double)')
      call c_c(CSQRT((1.,0.)*x),(1.,0.)*a,'CSQRT(complex)')
      call p_r_r(SQRT,x,a,'SQRT')
      call p_d_d(DSQRT,1.d0*x,1.d0*a,'DSQRT')
      call p_c_c(CSQRT,(1.,0.)*x,(1.,0.)*a ,'CSQRT')
      end
      subroutine failure(label)
!     Report failure and set flag
      character*(*) label
      logical fail
      common /flags/ fail
      write(6,'(a,a,a)') 'Test ',label,' FAILED'
      fail = .true.
      end
      subroutine c_r(a,b,label)
!     Check if REAL a equals b, and fail otherwise
      real a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0e-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end
      subroutine c_d(a,b,label)
!     Check if DOUBLE PRECISION a equals b, and fail otherwise
      double precision a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0d-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end
 
      subroutine c_c(a,b,label)
!     Check if COMPLEX a equals b, and fail otherwise
      complex a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0e-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end
      subroutine p_r_r(f,x,a,label)
!     Check if REAL f(x) equals a for REAL x
      real f,x,a
      character*(*) label
      call c_r(f(x),a,label)
      end
      subroutine p_d_d(f,x,a,label)
!     Check if DOUBLE PRECISION f(x) equals a for DOUBLE PRECISION x
      double precision f,x,a
      character*(*) label
      call c_d(f(x),a,label)
      end
      subroutine p_c_c(f,x,a,label)
!     Check if COMPLEX f(x) equals a for COMPLEX x
      complex f,x,a
      character*(*) label
      call c_c(f(x),a,label)
      end
