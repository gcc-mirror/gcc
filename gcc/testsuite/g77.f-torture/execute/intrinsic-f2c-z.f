c  intrinsic-f2c-z.f
c
c Test double complex intrinsics Z*.  
c These functions are f2c extensions
c
c     David Billinghurst <David.Billinghurst@riotinto.com>
c
      double complex z, a
      double precision x
      logical fail
      intrinsic zabs, zcos, zexp, zlog, zsin, zsqrt
      common /flags/ fail
      fail = .false.

c     ZABS - Absolute value
      z = (3.0d0,-4.0d0)
      x = 5.0d0
      call c_d(ZABS(z),x,'ZABS(double complex)')
      call p_d_z(ZABS,z,x,'ZABS')

c     ZCOS - Cosine
      z = (3.0d0,1.0d0)
      a = (-1.52763825012d0,-0.165844401919)
      call c_z(ZCOS(z),a,'ZCOS(double complex)')
      call p_z_z(ZCOS,z,a,'ZCOS')

c     ZEXP - Exponential
      z = (3.0d0,1.0d0)
      a = (10.8522619142d0,16.9013965352)
      call c_z(ZEXP(z),a,'ZEXP(double complex)')
      call p_z_z(ZEXP,z,a,'ZEXP')

c     ZLOG - Natural logarithm
      call c_z(ZLOG(a),z,'ZLOG(double complex)')
      call p_z_z(ZLOG,a,z,'ZLOG')

c     ZSIN - Sine
      z = (3.0d0,1.0d0)
      a = (0.217759551622d0,-1.1634403637d0)
      call c_z(ZSIN(z),a,'ZSIN(double complex)')
      call p_z_z(ZSIN,z,a,'ZSIN')

c     ZSQRT - Square root
      z = (0.0d0,-4.0d0)
      a = sqrt(2.0d0)*(1.0d0,-1.0d0)
      call c_z(ZSQRT(z),a,'ZSQRT(double complex)')
      call p_z_z(ZSQRT,z,a,'ZSQRT')

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

      subroutine c_z(a,b,label)
c     Check if DOUBLE COMPLEX a equals b, and fail otherwise
      double complex a, b
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

      subroutine p_z_z(f,x,a,label)
c     Check if DOUBLE COMPLEX f(x) equals a for DOUBLE COMPLEX x
      double complex f,x,a
      character*(*) label
      call c_z(f(x),a,label)
      end

      subroutine p_d_z(f,x,a,label)
c     Check if DOUBLE PRECISION f(x) equals a for DOUBLE COMPLEX x
      double precision f,x
      double complex a
      character*(*) label
      call c_d(f(x),a,label)
      end
