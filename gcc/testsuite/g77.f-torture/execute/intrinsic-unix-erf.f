c  intrinsic-unix-erf.f
c
c Test Bessel function intrinsics.  
c These functions are only available if provided by system
c
c     David Billinghurst <David.Billinghurst@riotinto.com>
c
      real x, a
      double precision dx, da
      logical fail
      common /flags/ fail
      fail = .false.

      x = 0.6
      dx = x 
c     ERF  - error function
      a = 0.6038561
      da = a
      call c_r(ERF(x),a,'ERF(real)')
      call c_d(ERF(dx),da,'ERF(double)')
      call c_d(DERF(dx),da,'DERF(double)')

c     ERFC  - complementary error function
      a = 1.0 - a
      da = a
      call c_r(ERFC(x),a,'ERFC(real)')
      call c_d(ERFC(dx),da,'ERFC(double)')
      call c_d(DERFC(dx),da,'DERFC(double)')

      if ( fail ) call abort()
      end

      subroutine failure(label)
c     Report failure and set flag
      character*(*) label
      logical fail
      common /flags/ fail
      write(6,'(a,a,a)') 'Test ',label,' FAILED'
      fail = .true.
      end

      subroutine c_r(a,b,label)
c     Check if REAL a equals b, and fail otherwise
      real a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0e-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end

      subroutine c_d(a,b,label)
c     Check if DOUBLE PRECISION a equals b, and fail otherwise
      double precision a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0d-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end
