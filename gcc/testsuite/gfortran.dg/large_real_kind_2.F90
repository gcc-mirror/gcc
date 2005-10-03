! { dg-do run }
! { dg-require-effective-target fortran_large_real }

! Testing library calls on large real kinds (larger than kind=8)
  implicit none

  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(8),parameter :: eps = 1e-8

  real(kind=k) :: x, x1
  real(8) :: y, y1
  complex(kind=k) :: z, z1
  complex(8) :: w, w1

#define TEST_FUNCTION(func,val) \
 x = val ;\
 y = x ;\
 x = func (x) ;\
 y = func (y) ;\
 if (abs((y - x) / y) > eps) call abort
  
#define CTEST_FUNCTION(func,valc) \
 z = valc ;\
 w = z ;\
 z = func (z) ;\
 w = func (w) ;\
 if (abs((z - w) / w) > eps) call abort

 TEST_FUNCTION(cos,17.456)
 TEST_FUNCTION(sin,17.456)
 TEST_FUNCTION(tan,1.456)
 TEST_FUNCTION(cosh,-2.45)
 TEST_FUNCTION(sinh,7.1)
 TEST_FUNCTION(tanh,12.7)
 TEST_FUNCTION(acos,0.78)
 TEST_FUNCTION(asin,-0.24)
 TEST_FUNCTION(atan,-17.123)
 TEST_FUNCTION(acosh,0.2)
 TEST_FUNCTION(asinh,0.3)
 TEST_FUNCTION(atanh,0.4)
 TEST_FUNCTION(exp,1.74)
 TEST_FUNCTION(log,0.00178914)
 TEST_FUNCTION(log10,123789.123)
 TEST_FUNCTION(sqrt,789.1356)
 TEST_FUNCTION(erf,1.45123231)
 TEST_FUNCTION(erfc,-0.123789)

 CTEST_FUNCTION(cos,(17.456,-1.123))
 CTEST_FUNCTION(sin,(17.456,-7.6))
 CTEST_FUNCTION(exp,(1.74,-1.01))
 CTEST_FUNCTION(log,(0.00178914,-1.207))
 CTEST_FUNCTION(sqrt,(789.1356,2.4))

#define TEST_POWER(val1,val2) \
 x = val1 ; \
 y = x ; \
 x1 = val2 ; \
 y1 = x1; \
 if (abs((x**x1 - y**y1)/(y**y1)) > eps) call abort
 
#define CTEST_POWER(val1,val2) \
 z = val1 ; \
 w = z ; \
 z1 = val2 ; \
 w1 = z1; \
 if (abs((z**z1 - w**w1)/(w**w1)) > eps) call abort

 CTEST_POWER (1.0,1.0)
 CTEST_POWER (1.0,5.4)
 CTEST_POWER (1.0,-5.4)
 CTEST_POWER (1.0,0.0)
 CTEST_POWER (-1.0,1.0)
 CTEST_POWER (-1.0,5.4)
 CTEST_POWER (-1.0,-5.4)
 CTEST_POWER (-1.0,0.0)
 CTEST_POWER (0.0,1.0)
 CTEST_POWER (0.0,5.4)
 CTEST_POWER (0.0,-5.4)
 CTEST_POWER (0.0,0.0)
 CTEST_POWER (7.6,1.0)
 CTEST_POWER (7.6,5.4)
 CTEST_POWER (7.6,-5.4)
 CTEST_POWER (7.6,0.0)
 CTEST_POWER (-7.6,1.0)
 CTEST_POWER (-7.6,5.4)
 CTEST_POWER (-7.6,-5.4)
 CTEST_POWER (-7.6,0.0)

 CTEST_POWER ((10.78,123.213),(14.123,13279.5))
 CTEST_POWER ((-10.78,123.213),(14.123,13279.5))
 CTEST_POWER ((10.78,-123.213),(14.123,13279.5))
 CTEST_POWER ((10.78,123.213),(-14.123,13279.5))
 CTEST_POWER ((10.78,123.213),(14.123,-13279.5))
 CTEST_POWER ((-10.78,-123.213),(14.123,13279.5))
 CTEST_POWER ((-10.78,123.213),(-14.123,13279.5))
 CTEST_POWER ((-10.78,123.213),(14.123,-13279.5))
 CTEST_POWER ((10.78,-123.213),(-14.123,13279.5))
 CTEST_POWER ((10.78,-123.213),(14.123,-13279.5))
 CTEST_POWER ((10.78,123.213),(-14.123,-13279.5))
 CTEST_POWER ((-10.78,-123.213),(-14.123,13279.5))
 CTEST_POWER ((-10.78,-123.213),(14.123,-13279.5))
 CTEST_POWER ((-10.78,123.213),(-14.123,-13279.5))
 CTEST_POWER ((10.78,-123.213),(-14.123,-13279.5))
 CTEST_POWER ((-10.78,-123.213),(-14.123,-13279.5))
 
end
