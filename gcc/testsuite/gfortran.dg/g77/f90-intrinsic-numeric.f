c { dg-do run }
c  f90-intrinsic-numeric.f
c
c Test Fortran 90 intrinsic numeric functions - Section 13.10.2 and 13.13 
c     David Billinghurst <David.Billinghurst@riotinto.com>
c
c Notes:
c  * g77 does not fully comply with F90.  Noncompliances noted in comments.
c  * Section 13.12: Specific names for intrinsic functions tested in
c intrinsic77.f

      logical fail
      integer*2 j, j2, ja
      integer*1 k, k2, ka

      common /flags/ fail
      fail = .false.

c     ABS - Section 13.13.1
      j = -9
      ja = 9
      k = j
      ka = ja
      call c_i(ABS(-7),7,'ABS(integer)')
      call c_i2(ABS(j),ja,'ABS(integer*2)')
      call c_i1(ABS(k),ka,'ABS(integer*1)')
      call c_r(ABS(-7.),7.,'ABS(real)')
      call c_d(ABS(-7.d0),7.d0,'ABS(double)')
      call c_r(ABS((3.,-4.)),5.0,'ABS(complex)')
      call c_d(ABS((3.d0,-4.d0)),5.0d0,'ABS(double complex)')

c     AIMAG - Section 13.13.6
      call c_r(AIMAG((2.,-7.)),-7.,'AIMAG(complex)')
c     g77: AIMAG(double complex) does not comply with F90
c     call c_d(AIMAG((2.d0,-7.d0)),-7.d0,'AIMAG(double complex)')

c     AINT - Section 13.13.7
      call c_r(AINT(2.783),2.0,'AINT(real) 1')
      call c_r(AINT(-2.783),-2.0,'AINT(real) 2')
      call c_d(AINT(2.783d0),2.0d0,'AINT(double precision) 1')
      call c_d(AINT(-2.783d0),-2.0d0,'AINT(double precision) 2')
c     Note:  g77 does not support optional argument KIND

c     ANINT - Section 13.13.10
      call c_r(ANINT(2.783),3.0,'ANINT(real) 1')
      call c_r(ANINT(-2.783),-3.0,'ANINT(real) 2')
      call c_d(ANINT(2.783d0),3.0d0,'ANINT(double precision) 1')
      call c_d(ANINT(-2.783d0),-3.0d0,'ANINT(double precision) 2')  
c     Note:  g77 does not support optional argument KIND

c     CEILING - Section 13.13.18
c     Not implemented

c     CMPLX - Section 13.13.20
      j = 1
      ja = 2
      k = 1
      ka = 2
      call c_c(CMPLX(1),(1.,0.),'CMPLX(integer)')
      call c_c(CMPLX(1,2),(1.,2.),'CMPLX(integer, integer)')
      call c_c(CMPLX(j),(1.,0.),'CMPLX(integer*2)')
      call c_c(CMPLX(j,ja),(1.,2.),'CMPLX(integer*2, integer*2)')
      call c_c(CMPLX(k),(1.,0.),'CMPLX(integer*1)')
      call c_c(CMPLX(k,ka),(1.,2.),'CMPLX(integer*1, integer*1)')
      call c_c(CMPLX(1.),(1.,0.),'CMPLX(real)')
      call c_c(CMPLX(1.d0),(1.,0.),'CMPLX(double)')
      call c_c(CMPLX(1.d0,2.d0),(1.,2.),'CMPLX(double,double)')
      call c_c(CMPLX(1.,2.),(1.,2.),'CMPLX(complex)')
      call c_c(CMPLX(1.d0,2.d0),(1.,2.),'CMPLX(double complex)')
c     NOTE: g77 does not support optional argument KIND
   
c     CONJG - Section 13.13.21
      call c_c(CONJG((2.,-7.)),(2.,7.),'CONJG(complex)')
      call c_z(CONJG((2.d0,-7.d0)),(2.d0,7.d0),'CONJG(double complex)')

c     DBLE - Section 13.13.27
      j = 5
      k = 5
      call c_d(DBLE(5),5.0d0,'DBLE(integer)')
      call c_d(DBLE(j),5.0d0,'DBLE(integer*2)')
      call c_d(DBLE(k),5.0d0,'DBLE(integer*1)')
      call c_d(DBLE(5.),5.0d0,'DBLE(real)')
      call c_d(DBLE(5.0d0),5.0d0,'DBLE(double)')
      call c_d(DBLE((5.0,0.5)),5.0d0,'DBLE(complex)')
      call c_d(DBLE((5.0d0,0.5d0)),5.0d0,'DBLE(double complex)')

c     DIM - Section 13.13.29
      j = -8
      j2 = -3
      ja = 0
      k = -8
      k2 = -3
      ka = 0
      call c_i(DIM(-8,-3),0,'DIM(integer)')
      call c_i2(DIM(j,j2),ja,'DIM(integer*2)')
      call c_i1(DIM(k,k2),ka,'DIM(integer*1)')
      call c_r(DIM(-8.,-3.),0.,'DIM(real,real)')
      call c_d(DIM(-8.d0,-3.d0),0.d0,'DIM(double,double)')
 
c     DPROD - Section 13.13.31
      call c_d(DPROD(-8.,-3.),24.d0,'DPROD(real,real)')
     
c     FLOOR - Section 13.13.36
c     Not implemented

c     INT - Section 13.13.47
      j = 5
      k = 5
      call c_i(INT(5),5,'INT(integer)')
      call c_i(INT(j),5,'INT(integer*2)')
      call c_i(INT(k),5,'INT(integer*1)')
      call c_i(INT(5.01),5,'INT(real)')
      call c_i(INT(5.01d0),5,'INT(double)')
c     Note: Does not accept optional second argument KIND

c     MAX - Section 13.13.63
      j = 1
      j2 = 2
      ja = 2
      k = 1
      k2 = 2
      ka = 2
      call c_i(MAX(1,2,3),3,'MAX(integer,integer,integer)')
      call c_i2(MAX(j,j2),ja,'MAX(integer*2,integer*2)')
      call c_i1(MAX(k,k2),ka,'MAX(integer*1,integer*1)')
      call c_r(MAX(1.,2.,3.),3.,'MAX(real,real,real)')
      call c_d(MAX(1.d0,2.d0,3.d0),3.d0,'MAX(double,double,double)')

c     MIN - Section 13.13.68
      j = 1
      j2 = 2
      ja = 1
      k = 1
      k2 = 2
      ka = 1
      call c_i(MIN(1,2,3),1,'MIN(integer,integer,integer)')
      call c_i2(MIN(j,j2),ja,'MIN(integer*2,integer*2)')
      call c_i1(MIN(k,k2),ka,'MIN(integer*1,integer*1)')
      call c_r(MIN(1.,2.,3.),1.,'MIN(real,real,real)')
      call c_d(MIN(1.d0,2.d0,3.d0),1.d0,'MIN(double,double,double)')

c     MOD - Section 13.13.72
      call c_i(MOD(8,5),3,'MOD(integer,integer) 1')
      call c_i(MOD(-8,5),-3,'MOD(integer,integer) 2')
      call c_i(MOD(8,-5),3,'MOD(integer,integer) 3')
      call c_i(MOD(-8,-5),-3,'MOD(integer,integer) 4')
      j = 8
      j2 = 5
      ja = 3
      call c_i2(MOD(j,j2),ja,'MOD(integer*2,integer*2) 1')
      call c_i2(MOD(-j,j2),-ja,'MOD(integer*2,integer*2) 2')
      call c_i2(MOD(j,-j2),ja,'MOD(integer*2,integer*2) 3')
      call c_i2(MOD(-j,-j2),-ja,'MOD(integer*2,integer*2) 4')
      k = 8
      k2 = 5
      ka = 3
      call c_i1(MOD(k,k2),ka,'MOD(integer*1,integer*1) 1')
      call c_i1(MOD(-k,k2),-ka,'MOD(integer*1,integer*1) 2')
      call c_i1(MOD(k,-k2),ka,'MOD(integer*1,integer*1) 3')
      call c_i1(MOD(-k,-k2),-ka,'MOD(integer*1,integer*1) 4')
      call c_r(MOD(8.,5.),3.,'MOD(real,real) 1')
      call c_r(MOD(-8.,5.),-3.,'MOD(real,real) 2')
      call c_r(MOD(8.,-5.),3.,'MOD(real,real) 3')
      call c_r(MOD(-8.,-5.),-3.,'MOD(real,real) 4')
      call c_d(MOD(8.d0,5.d0),3.d0,'MOD(double,double) 1')
      call c_d(MOD(-8.d0,5.d0),-3.d0,'MOD(double,double) 2')
      call c_d(MOD(8.d0,-5.d0),3.d0,'MOD(double,double) 3')
      call c_d(MOD(-8.d0,-5.d0),-3.d0,'MOD(double,double) 4')

c     MODULO - Section 13.13.73
c     Not implemented

c     NINT - Section 13.13.76
      call c_i(NINT(2.783),3,'NINT(real)')
      call c_i(NINT(2.783d0),3,'NINT(double)')
c     Optional second argument KIND not implemented

c     REAL - Section 13.13.86
      j = -2
      k = -2
      call c_r(REAL(-2),-2.0,'REAL(integer)')
      call c_r(REAL(j),-2.0,'REAL(integer*2)')
      call c_r(REAL(k),-2.0,'REAL(integer*1)')
      call c_r(REAL(-2.0),-2.0,'REAL(real)')
      call c_r(REAL(-2.0d0),-2.0,'REAL(double)')
      call c_r(REAL((-2.,9.)),-2.0,'REAL(complex)')
c     REAL(double complex) not implemented
c     call c_r(REAL((-2.d0,9.d0)),-2.0,'REAL(double complex)')

c     SIGN - Section 13.13.96
      j = -3
      j2 = 2
      ja = 3
      k = -3
      k2 = 2
      ka = 3
      call c_i(SIGN(-3,2),3,'SIGN(integer)')
      call c_i2(SIGN(j,j2),ja,'SIGN(integer*2)')
      call c_i1(SIGN(k,k2),ka,'SIGN(integer*1)')
      call c_r(SIGN(-3.0,2.),3.,'SIGN(real,real)')
      call c_d(SIGN(-3.d0,2.d0),3.d0,'SIGN(double,double)')
 
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

      subroutine c_i(i,j,label)
c     Check if INTEGER i equals j, and fail otherwise
      integer i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

      subroutine c_i2(i,j,label)
c     Check if INTEGER*2 i equals j, and fail otherwise
      integer*2 i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

      subroutine c_i1(i,j,label)
c     Check if INTEGER*1 i equals j, and fail otherwise
      integer*1 i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
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
