      program intrinsic77
c
c  Test Fortran 77 intrinsic functions (ANSI X3.9-1978 Section 15.10)
c 
c  Test:
c  *  specific functions
c  *  generic functions with each argument type
c  *  specific functions by passing as subroutine argument
c     where permiited by Section 13.12 of Fortran 90 standard
c
      logical fail
      common /flags/ fail

      fail = .false.
      call type_conversion
      call truncation
      call nearest_whole_number
      call nearest_integer
      call absolute_value
      call remaindering
      call transfer_of_sign
      call positive_difference
      call double_precision_product
      call choosing_largest_value
      call choosing_smallest_value
      call length_of_character_array
      call index_of_substring
      call imaginary_part
      call complex_conjugate
      call square_root
      call exponential
      call natural_logarithm
      call common_logarithm
      call sine
      call cosine
      call tangent
      call arcsine
      call arccosine
      call arctangent
      call hyperbolic_sine
      call hyperbolic_cosine
      call hyperbolic_tangent
      call lexically_greater_than_or_equal
      call lexically_greater_than
      call lexically_less_than_or_equal
      call lexically_less_than

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

      subroutine c_l(a,b,label)
c     Check if LOGICAL a equals b, and fail otherwise
      logical a, b
      character*(*) label
      if ( a .neqv. b ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end

      subroutine c_ch(a,b,label)
c     Check if CHARACTER a equals b, and fail otherwise
      character*(*) a, b
      character*(*) label
      if ( a .ne. b ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end

      subroutine p_i_i(f,x,i,label)
c     Check if INTEGER f(x) equals i for INTEGER x
      integer f,x,i
      character*(*) label
      call c_i(f(x),i,label)
      end

      subroutine p_i_ii(f,x1,x2,i,label)
c     Check if INTEGER f(x1,x2) equals i for INTEGER x
      integer f,x1,x2,i
      character*(*) label
      call c_i(f(x1,x2),i,label)
      end

      subroutine p_i_r(f,x,i,label)
c     Check if INTEGER f(x) equals i for REAL x
      real x
      integer f,i
      character*(*) label
      call c_i(f(x),i,label)
      end

      subroutine p_i_d(f,x,i,label)
c     Check if INTEGER f(x) equals i for DOUBLE PRECISION x
      double precision x
      integer f,i
      character*(*) label
      call c_i(f(x),i,label)
      end

      subroutine p_i_ch(f,x,a,label)
c     Check if INTEGER f(x) equals a for CHARACTER x
      character*(*) x
      integer f, a
      character*(*) label
      call c_i(f(x),a,label)
      end

      subroutine p_i_chch(f,x1,x2,a,label)
c     Check if INTEGER f(x1,x2) equals a for CHARACTER x1 and x2
      character*(*) x1,x2
      integer f, a
      character*(*) label
      call c_i(f(x1,x2),a,label)
      end

      subroutine p_r_r(f,x,a,label)
c     Check if REAL f(x) equals a for REAL x
      real f,x,a
      character*(*) label
      call c_r(f(x),a,label)
      end

      subroutine p_r_rr(f,x1,x2,a,label)
c     Check if REAL f(x1,x2) equals a for REAL x1, x2
      real f,x1,x2,a
      character*(*) label
      call c_r(f(x1,x2),a,label)
      end

      subroutine p_d_d(f,x,a,label)
c     Check if DOUBLE PRECISION f(x) equals a for DOUBLE PRECISION x
      double precision f,x,a
      character*(*) label
      call c_d(f(x),a,label)
      end

      subroutine p_d_rr(f,x1,x2,a,label)
c     Check if DOUBLE PRECISION f(x1,x2) equals a for real x1,x2
      double precision f,a
      real x1,x2
      character*(*) label
      call c_d(f(x1,x2),a,label)
      end

      subroutine p_d_dd(f,x1,x2,a,label)
c     Check if DOUBLE PRECISION f(x1,x2) equals a for DOUBLE PRECISION x1,x2
      double precision f,x1,x2,a
      character*(*) label
      call c_d(f(x1,x2),a,label)
      end

      subroutine p_c_c(f,x,a,label)
c     Check if COMPLEX f(x) equals a for COMPLEX x
      complex f,x,a
      character*(*) label
      call c_c(f(x),a,label)
      end

      subroutine p_r_c(f,x,a,label)
c     Check if REAL f(x) equals a for COMPLEX x
      complex x
      real f, a
      character*(*) label
      call c_r(f(x),a,label)
      end

      subroutine type_conversion
      integer i
      character*1 c
c     conversion to integer
      call c_i(INT(5),5,'INT(integer)')
      call c_i(INT(5.01),5,'INT(real)')
      call c_i(INT(5.01d0),5,'INT(double)')
      call c_i(INT((5.01,-3.0)),5,'INT(complex)')
      call c_i(IFIX(5.01),5,'IFIX(real)')
      call c_i(IDINT(5.01d0),5,'IDINT(double)')
c     conversion to real
      call c_r(REAL(-2),-2.0,'REAL(integer)')
      call c_r(REAL(-2.0),-2.0,'REAL(real)')
      call c_r(REAL(-2.0d0),-2.0,'REAL(double)')
      call c_r(REAL((-2.,9.)),-2.0,'REAL(complex)')
      call c_r(FLOAT(-2),-2.0,'FLOAT(int)')
      call c_r(SNGL(-2.0d0),-2.0,'SNGL(double)')
c     conversion to double
      call c_d(DBLE(5),5.0d0,'DBLE(integer)')
      call c_d(DBLE(5.),5.0d0,'DBLE(real)')
      call c_d(DBLE(5.0d0),5.0d0,'DBLE(double)')
      call c_d(DBLE((5.0,0.5)),5.0d0,'DBLE(complex)')
c     conversion to complex
      call c_c(CMPLX(1),(1.,0.),'CMPLX(integer)')
      call c_c(CMPLX(1,2),(1.,2.),'CMPLX(integer, integer)')
      call c_c(CMPLX(1.),(1.,0.),'CMPLX(real)')
      call c_c(CMPLX(1.,2.),(1.,2.),'CMPLX(real,real)')
      call c_c(CMPLX(1.d0),(1.,0.),'CMPLX(double)')
      call c_c(CMPLX(1.d0,2.d0),(1.,2.),'CMPLX(double,double)')
      call c_c(CMPLX(1.,2.),(1.,2.),'CMPLX(complex)')
c     character conversion
      c = 'C'
      i = ichar(c)
      call c_i(ICHAR(c),i,'ICHAR')
      call c_ch(CHAR(i),c,'CHAR')
      end

      subroutine truncation
      intrinsic aint, dint
      call c_r(AINT(9.2),9.0,'AINT(real)')
      call c_d(AINT(9.2d0),9.0d0,'AINT(double)')
      call c_d(DINT(9.2d0),9.0d0,'DINT(double)')
      call p_r_r(AINT,9.2,9.0,'AINT')
      call p_d_d(DINT,9.2d0,9.0d0,'DINT')
      end

      subroutine nearest_whole_number
      intrinsic anint, dnint
      call c_r(ANINT(9.2),9.0,'ANINT(real)')
      call c_d(ANINT(9.2d0),9.0d0,'ANINT(double)')
      call c_d(DNINT(9.2d0),9.0d0,'DNINT(double)')
      call p_r_r(ANINT,9.2,9.0,'ANINT')
      call p_d_d(DNINT,9.2d0,9.0d0,'DNINT')
      end

      subroutine nearest_integer
      intrinsic nint, idnint
      call c_i(NINT(9.2),9,'NINT(real)')
      call c_i(NINT(9.2d0),9,'NINT(double)')
      call c_i(IDNINT(9.2d0),9,'IDNINT(double)')
      call p_i_r(NINT,9.2,9,'NINT')
      call p_i_d(IDNINT,9.2d0,9,'IDNINT')
      end

      subroutine absolute_value
      intrinsic iabs, abs, dabs, cabs
      call c_i(ABS(-7),7,'ABS(integer)')
      call c_r(ABS(-7.),7.,'ABS(real)')
      call c_d(ABS(-7.d0),7.d0,'ABS(double)')
      call c_r(ABS((3.,-4.)),5.0,'ABS(complex)')
      call c_i(IABS(-7),7,'IABS(integer)')
      call c_d( DABS(-7.d0),7.d0,'DABS(double)')
      call c_r( CABS((3.,-4.)),5.0,'CABS(complex)')
      call p_i_i(IABS,-7,7,'IABS')
      call p_r_r(ABS,-7.,7.,'ABS')
      call p_d_d(DABS,-7.0d0,7.0d0,'DABS')
      call p_r_c(CABS,(3.,-4.), 5.0,'CABS')
      end

      subroutine remaindering
      intrinsic mod, amod, dmod
      call c_i( MOD(8,3),2,'MOD(integer,integer)')
      call c_r( MOD(8.,3.),2.,'MOD(real,real)')
      call c_d( MOD(8.d0,3.d0),2.d0,'MOD(double,double)')
      call c_r( AMOD(8.,3.),2.,'AMOD(real,real)')
      call c_d( DMOD(8.d0,3.d0),2.d0,'DMOD(double,double)')
      call p_i_ii(MOD,8,3,2,'MOD')
      call p_r_rr(AMOD,8.,3.,2.,'AMOD')
      call p_d_dd(DMOD,8.d0,3.d0,2.d0,'DMOD')
      end

      subroutine transfer_of_sign
      intrinsic isign,sign,dsign
      call c_i(SIGN(8,-3),-8,'SIGN(integer)')
      call c_r(SIGN(8.,-3.),-8.,'SIGN(real,real)')
      call c_d(SIGN(8.d0,-3.d0),-8.d0,'SIGN(double,double)')
      call c_i(ISIGN(8,-3),-8,'ISIGN(integer)')
      call c_d(DSIGN(8.d0,-3.d0),-8.d0,'DSIGN(double,double)')
      call p_i_ii(ISIGN,8,-3,-8,'ISIGN')
      call p_r_rr(SIGN,8.,-3.,-8.,'SIGN')
      call p_d_dd(DSIGN,8.d0,-3.d0,-8.d0,'DSIGN')
      end

      subroutine positive_difference
      intrinsic idim, dim, ddim
      call c_i(DIM(-8,-3),0,'DIM(integer)')
      call c_r(DIM(-8.,-3.),0.,'DIM(real,real)')
      call c_d(DIM(-8.d0,-3.d0),0.d0,'DIM(double,double)')
      call c_i(IDIM(-8,-3),0,'IDIM(integer)')
      call c_d(DDIM(-8.d0,-3.d0),0.d0,'DDIM(double,double)')
      call p_i_ii(IDIM,-8,-3,0,'IDIM')
      call p_r_rr(DIM,-8.,-3.,0.,'DIM')
      call p_d_dd(DDIM,-8.d0,-3.d0,0.d0,'DDIM')
      end

      subroutine double_precision_product
      intrinsic dprod
      call c_d(DPROD(-8.,-3.),24.d0,'DPROD(real,real)')
      call p_d_rr(DPROD,-8.,-3.,24.d0,'DPROD')
      end

      subroutine choosing_largest_value
      call c_i(MAX(1,2,3),3,'MAX(integer,integer,integer)')
      call c_r(MAX(1.,2.,3.),3.,'MAX(real,real,real)')
      call c_d(MAX(1.d0,2.d0,3.d0),3.d0,'MAX(double,double,double)')
      call c_i(MAX0(1,2,3),3,'MAX0(integer,integer,integer)')
      call c_r(AMAX1(1.,2.,3.),3.,'MAX(real,real,real)')
      call c_d(DMAX1(1.d0,2.d0,3.d0),3.d0,'DMAX1(double,double,double)')
      call c_r(AMAX0(1,2,3),3.,'AMAX0(integer,integer,integer)')
      call c_i(MAX1(1.,2.,3.),3,'MAX1(real,real,real)')
      end

      subroutine choosing_smallest_value
      call c_i(MIN(1,2,3),1,'MIN(integer,integer,integer)')
      call c_r(MIN(1.,2.,3.),1.,'MIN(real,real,real)')
      call c_d(MIN(1.d0,2.d0,3.d0),1.d0,'MIN(double,double,double)')
      call c_i(MIN0(1,2,3),1,'MIN0(integer,integer,integer)')
      call c_r(AMIN1(1.,2.,3.),1.,'MIN(real,real,real)')
      call c_d(DMIN1(1.d0,2.d0,3.d0),1.d0,'DMIN1(double,double,double)')
      call c_r(AMIN0(1,2,3),1.,'AMIN0(integer,integer,integer)')
      call c_i(MIN1(1.,2.,3.),1,'MIN1(real,real,real)')
      end

      subroutine length_of_character_array
      intrinsic len
      call c_i(LEN('ABCDEF'),6,'LEN 1')
      call p_i_ch(LEN,'ABCDEF',6,'LEN 2')
      end

      subroutine index_of_substring
      intrinsic index
      call c_i(INDEX('ABCDEF','C'),3,'INDEX 1')
      call p_i_chch(INDEX,'ABCDEF','C',3,'INDEX 2')
      end

      subroutine imaginary_part
      intrinsic aimag
      call c_r(AIMAG((2.,-7.)),-7.,'AIMAG(complex)')
      call p_r_c(AIMAG,(2.,-7.),-7.,'AIMAG(complex)')
      end

      subroutine complex_conjugate
      intrinsic conjg
      call c_c(CONJG((2.,-7.)),(2.,7.),'CONJG(complex)')
      call p_c_c(CONJG,(2.,-7.),(2.,7.),'CONJG')
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

      subroutine exponential
      intrinsic exp, dexp, cexp
      real x, a
      x = 0.0
      a = 1.0
      call c_r(EXP(x),a,'EXP(real)')
      call c_d(EXP(1.d0*x),1.d0*a,'EXP(double)')
      call c_c(EXP((1.,0.)*x),(1.,0.)*a,'EXP(complex)')
      call c_d(DEXP(1.d0*x),1.d0*a,'DEXP(double)')
      call c_c(CEXP((1.,0.)*x),(1.,0.)*a,'CEXP(complex)')
      call p_r_r(EXP,x,a,'EXP')
      call p_d_d(DEXP,1.d0*x,1.d0*a,'DEXP')
      call p_c_c(CEXP,(1.,0.)*x,(1.,0.)*a ,'CEXP')
      end

      subroutine natural_logarithm
      intrinsic alog, dlog, clog
      real x, a
      a = 1.234
      x = exp(a)
      call c_r(LOG(x),a,'LOG(real)')
      call c_d(LOG(1.d0*x),1.d0*a,'LOG(double)')
      call c_c(LOG((1.,0.)*x),(1.,0.)*a,'LOG(complex)')
      call c_r(ALOG(x),a,'ALOG(real)')
      call c_d(DLOG(1.d0*x),1.d0*a,'DLOG(double)')
      call c_c(CLOG((1.,0.)*x),(1.,0.)*a,'CLOG(complex)')
      call p_r_r(ALOG,x,a,'LOG')
      call p_d_d(DLOG,1.d0*x,1.d0*a,'DLOG')
      call p_c_c(CLOG,(1.,0.)*x,(1.,0.)*a,'CLOG')
      end

      subroutine common_logarithm
      intrinsic alog10, dlog10
      real x, a
      x = 100.0
      a = 2.0
      call c_r(LOG10(x),a,'LOG10(real)')
      call c_d(LOG10(1.d0*x),1.d0*a,'LOG10(double)')
      call c_r(ALOG10(x),a,'ALOG10(real)')
      call c_d(DLOG10(1.d0*x),1.d0*a,'DLOG10(double)')
      call p_r_r(ALOG10,x,a,'ALOG10')
      call p_d_d(DLOG10,1.d0*x,1.d0*a ,'DLOG10')
      end

      subroutine sine
      intrinsic sin, dsin, csin
      real x, a
      a = 1.0
      x = asin(a)
      call c_r(SIN(x),a,'SIN(real)')
      call c_d(SIN(1.d0*x),1.d0*a,'SIN(double)')
      call c_c(SIN((1.,0.)*x),(1.,0.)*a,'SIN(complex)')
      call c_d(DSIN(1.d0*x),1.d0*a,'DSIN(double)')
      call c_c(CSIN((1.,0.)*x),(1.,0.)*a,'CSIN(complex)')
      call p_r_r(SIN,x,a,'SIN')
      call p_d_d(DSIN,1.d0*x,1.d0*a,'DSIN')
      call p_c_c(CSIN,(1.,0.)*x,(1.,0.)*a ,'CSIN')
      end

      subroutine cosine
      intrinsic cos, dcos, ccos
      real x, a
      a = 0.123456
      x = acos(a)
      call c_r(COS(x),a,'COS(real)')
      call c_d(COS(1.d0*x),1.d0*a,'COS(double)')
      call c_c(COS((1.,0.)*x),(1.,0.)*a,'COS(complex)')
      call c_r(COS(x),a,'COS(real)')
      call c_d(DCOS(1.d0*x),1.d0*a,'DCOS(double)')
      call c_c(CCOS((1.,0.)*x),(1.,0.)*a,'CCOS(complex)')
      call p_r_r(COS,x,a,'COS')
      call p_d_d(DCOS,1.d0*x,1.d0*a ,'DCOS')
      call p_c_c(CCOS,(1.,0.)*x, (1.,0.)*a ,'CCOS')
      end

      subroutine tangent
      intrinsic tan, dtan
      real x, a
      a = 0.5
      x = atan(a)
      call c_r(TAN(x),a,'TAN(real)')
      call c_d(TAN(1.d0*x),1.d0*a,'TAN(double)')
      call c_d(DTAN(1.d0*x),1.d0*a,'DTAN(double)')
      call p_r_r(TAN,x,a,'TAN')
      call p_d_d(DTAN,1.d0*x,1.d0*a ,'DTAN')
      end

      subroutine arcsine
      intrinsic asin, dasin
      real x, a
      a = 0.5
      x = sin(a)
      call c_r(ASIN(x),a,'ASIN(real)')
      call c_d(ASIN(1.d0*x),1.d0*a,'ASIN(double)')
      call c_d(DASIN(1.d0*x),1.d0*a,'DASIN(double)')
      call p_r_r(ASIN,x,a,'ASIN')
      call p_d_d(DASIN,1.d0*x,1.d0*a ,'DASIN')
      end

      subroutine arccosine
      intrinsic acos, dacos
      real x, a
      x = 0.70710678
      a = 0.785398
      call c_r(ACOS(x),a,'ACOS(real)')
      call c_d(ACOS(1.d0*x),1.d0*a,'ACOS(double)')
      call c_d(DACOS(1.d0*x),1.d0*a,'DACOS(double)')
      call p_r_r(ACOS,x,a,'ACOS')
      call p_d_d(DACOS,1.d0*x,1.d0*a ,'DACOS')
      end

      subroutine arctangent
      intrinsic atan, atan2, datan, datan2
      real x1, x2, a
      a = 0.75
      x1 = tan(a)
      x2 = 1.0
      call c_r(ATAN(x1),a,'ATAN(real)')
      call c_d(ATAN(1.d0*x1),1.d0*a,'ATAN(double)')
      call c_d(DATAN(1.d0*x1),1.d0*a,'DATAN(double)')
      call c_r(ATAN2(x1,x2),a,'ATAN2(real)')
      call c_d(ATAN2(1.d0*x1,1.d0*x2),1.d0*a,'ATAN2(double)')
      call c_d(DATAN2(1.d0*x1,1.d0*x2),1.0d0*a,'DATAN2(double)')
      call p_r_r(ATAN,x1,a,'ATAN')
      call p_d_d(DATAN,1.d0*x1,1.d0*a,'DATAN')
      call p_r_rr(ATAN2,x1,x2,a,'ATAN2')
      call p_d_dd(DATAN2,1.d0*x1,1.d0*x2,1.d0*a,'DATAN2')
      end

      subroutine hyperbolic_sine
      intrinsic sinh, dsinh
      real x, a
      x = 1.0
      a = 1.1752012
      call c_r(SINH(x),a,'SINH(real)')
      call c_d(SINH(1.d0*x),1.d0*a,'SINH(double)')
      call c_d(DSINH(1.d0*x),1.d0*a,'DSINH(double)')
      call p_r_r(SINH,x,a,'SINH')
      call p_d_d(DSINH,1.d0*x,1.d0*a ,'DSINH')
      end

      subroutine hyperbolic_cosine
      intrinsic cosh, dcosh
      real x, a
      x = 1.0
      a = 1.5430806
      call c_r(COSH(x),a,'COSH(real)')
      call c_d(COSH(1.d0*x),1.d0*a,'COSH(double)')
      call c_d(DCOSH(1.d0*x),1.d0*a,'DCOSH(double)')
      call p_r_r(COSH,x,a,'COSH')
      call p_d_d(DCOSH,1.d0*x,1.d0*a ,'DCOSH')
      end

      subroutine hyperbolic_tangent
      intrinsic tanh, dtanh
      real x, a
      x = 1.0
      a = 0.76159416
      call c_r(TANH(x),a,'TANH(real)')
      call c_d(TANH(1.d0*x),1.d0*a,'TANH(double)')
      call c_d(DTANH(1.d0*x),1.d0*a,'DTANH(double)')
      call p_r_r(TANH,x,a,'TANH')
      call p_d_d(DTANH,1.d0*x,1.d0*a ,'DTANH')
      end

      subroutine lexically_greater_than_or_equal
      call c_l(LGE('A','B'),.FALSE.,'LGE(character,character) 1')
      call c_l(LGE('B','A'),.TRUE.,'LGE(character,character) 2')
      call c_l(LGE('A','A'),.TRUE.,'LGE(character,character) 3')
      end

      subroutine lexically_greater_than
      call c_l(LGT('A','B'),.FALSE.,'LGT(character,character) 1')
      call c_l(LGT('B','A'),.TRUE.,'LGT(character,character) 2')
      call c_l(LGT('A','A'),.FALSE.,'LGT(character,character) 3')
      end

      subroutine lexically_less_than_or_equal
      call c_l(LLE('A','B'),.TRUE.,'LLE(character,character) 1')
      call c_l(LLE('B','A'),.FALSE.,'LLE(character,character) 2')
      call c_l(LLE('A','A'),.TRUE.,'LLE(character,character) 3')
      end

      subroutine lexically_less_than
      call c_l(LLT('A','B'),.TRUE.,'LLT(character,character) 1')
      call c_l(LLT('B','A'),.FALSE.,'LLT(character,character) 2')
      call c_l(LLT('A','A'),.FALSE.,'LLT(character,character) 3')
      end
