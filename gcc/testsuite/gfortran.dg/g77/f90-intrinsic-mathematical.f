c { dg-do run }
c  f90-intrinsic-mathematical.f
c
c Test Fortran 90 intrinsic mathematical functions - Section 13.10.3 and
c 13.13 
c     David Billinghurst <David.Billinghurst@riotinto.com>
c
c Notes:
c  * g77 does not fully comply with F90.  Noncompliances noted in comments.
c  * Section 13.12: Specific names for intrinsic functions tested in
c intrinsic77.f

      logical fail
      common /flags/ fail
      fail = .false.

c     ACOS - Section 13.13.3
      call c_r(ACOS(0.54030231),1.0,'ACOS(real)')
      call c_d(ACOS(0.54030231d0),1.d0,'ACOS(double)')

c     ASIN - Section 13.13.12
      call c_r(ASIN(0.84147098),1.0,'ASIN(real)')
      call c_d(ASIN(0.84147098d0),1.d0,'ASIN(double)')

c     ATAN - Section 13.13.14
      call c_r(ATAN(1.5574077),1.0,'ATAN(real)')
      call c_d(ATAN(1.5574077d0),1.d0,'ATAN(double)')
      
c     ATAN2 - Section 13.13.15
      call c_r(ATAN2(1.5574077,1.),1.0,'ATAN2(real)')
      call c_d(ATAN2(1.5574077d0,1.d0),1.d0,'ATAN2(double)')

c     COS - Section 13.13.22
      call c_r(COS(1.0),0.54030231,'COS(real)')
      call c_d(COS(1.d0),0.54030231d0,'COS(double)')
      call c_c(COS((1.,0.)),(0.54030231,0.),'COS(complex)')
      call c_z(COS((1.d0,0.d0)),(0.54030231d0,0.d0),
     $     'COS(double complex)')

c     COSH - Section 13.13.23
      call c_r(COSH(1.0),1.5430806,'COSH(real)')
      call c_d(COSH(1.d0),1.5430806d0,'COSH(double)')

c     EXP - Section 13.13.34
      call c_r(EXP(1.0),2.7182818,'EXP(real)')
      call c_d(EXP(1.d0),2.7182818d0,'EXP(double)')
      call c_c(EXP((1.,0.)),(2.7182818,0.),'EXP(complex)')
      call c_z(EXP((1.d0,0.d0)),(2.7182818d0,0.d0),
     $     'EXP(double complex)')

c     LOG - Section 13.13.59
      call c_r(LOG(10.0),2.3025851,'LOG(real)')
      call c_d(LOG(10.d0),2.3025851d0,'LOG(double)')
      call c_c(LOG((10.,0.)),(2.3025851,0.),'LOG(complex)')
      call c_z(LOG((10.d0,0.)),(2.3025851d0,0.d0),
     $     'LOG(double complex)')

c     LOG10 - Section 13.13.60
      call c_r(LOG10(10.0),1.0,'LOG10(real)')
      call c_d(LOG10(10.d0),1.d0,'LOG10(double)')

c     SIN - Section 13.13.97
      call c_r(SIN(1.0),0.84147098,'SIN(real)')
      call c_d(SIN(1.d0),0.84147098d0,'SIN(double)')
      call c_c(SIN((1.,0.)),(0.84147098,0.),'SIN(complex)')
      call c_z(SIN((1.d0,0.d0)),(0.84147098d0,0.d0),
     $     'SIN(double complex)')

c     SINH - Section 13.13.98
      call c_r(SINH(1.0),1.175201,'SINH(real)')
      call c_d(SINH(1.d0),1.175201d0,'SINH(double)')

c     SQRT - Section 13.13.102
      call c_r(SQRT(4.0),2.0,'SQRT(real)')
      call c_d(SQRT(4.d0),2.d0,'SQRT(double)')
      call c_c(SQRT((4.,0.)),(2.,0.),'SQRT(complex)')
      call c_z(SQRT((4.d0,0.)),(2.d0,0.),
     $     'SQRT(double complex)')
 
c     TAN - Section 13.13.105
      call c_r(TAN(1.0),1.5574077,'TAN(real)')
      call c_d(TAN(1.d0),1.5574077d0,'TAN(double)')
     
c     TANH - Section 13.13.106
      call c_r(TANH(1.0),0.76159416,'TANH(real)')
      call c_d(TANH(1.d0),0.76159416d0,'TANH(double)')

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

      subroutine c_c(a,b,label)
c     Check if COMPLEX a equals b, and fail otherwise
      complex a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0e-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end

      subroutine c_z(a,b,label)
c     Check if COMPLEX a equals b, and fail otherwise
      double complex a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0d-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end
