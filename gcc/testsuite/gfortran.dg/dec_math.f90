! { dg-options "-cpp -std=gnu" }
! { dg-do run { xfail i?86-*-freebsd* } }
!
! Test extra math intrinsics formerly offered by -fdec-math,
! now included with -std=gnu or -std=legacy.
!

module dec_math

  implicit none

  real(4), parameter :: pi_f = 3.14159274_4
  real(8), parameter :: pi_d = 3.1415926535897931_8
#ifdef __GFC_REAL_10__
  real(10), parameter :: pi_l = 3.1415926535897932383_10
#endif
#ifdef __GFC_REAL_16__
  real(16), parameter :: pi_q = 3.1415926535897932384626433832795028_16
#endif

  real(4), parameter :: r2d_f = 180.0_4 / pi_f
  real(8), parameter :: r2d_d = 180.0_8 / pi_d
#ifdef __GFC_REAL_10__
  real(10), parameter :: r2d_l = 180.0_10 / pi_l
#endif
#ifdef __GFC_REAL_16__
  real(16), parameter :: r2d_q = 180.0_16 / pi_q
#endif

contains

  function d2rf(x)
    implicit none
    real(4), intent(in) :: x
    real(4) :: d2rf
    d2rf = (x * pi_f) / 180.0_4
  endfunction

  subroutine cmpf(x, f1, f2, tolerance, str)
    implicit none
    real(4), intent(in) :: x, f1, f2, tolerance
    character(len=*), intent(in) :: str
    if ( abs(f2 - f1) .gt. tolerance ) then
      write (*, '(A,A,F12.6,A,F12.6,F12.6)') str, "(", x, ")", f1, f2
      STOP 1
    endif
  endsubroutine

  function d2rd(x)
    implicit none
    real(8), intent(in) :: x
    real(8) :: d2rd
    d2rd = (x * pi_d) / 180.0_8
  endfunction

  subroutine cmpd(x, d1, d2, tolerance, str)
    implicit none
    real(8), intent(in) :: x, d1, d2, tolerance
    character(len=*), intent(in) :: str
    if ( dabs(d2 - d1) .gt. tolerance ) then
      write (*, '(A,A,F18.14,A,F18.14,F18.14)') str, "(", x, ")", d1, d2
      STOP 2
    endif
  endsubroutine

#ifdef __GFC_REAL_10__
  function d2rl(x)
    implicit none
    real(10), intent(in) :: x
    real(10) :: d2rl
    d2rl = (x * pi_l) / 180.0_10
  endfunction

  subroutine cmpl(x, f1, f2, tolerance, str)
    implicit none
    real(10), intent(in) :: x, f1, f2, tolerance
    character(len=*), intent(in) :: str
    if ( abs(f2 - f1) .gt. tolerance ) then
      write (*, '(A,A,F21.17,A,F21.17,F21.17)') str, "(", x, ")", f1, f2
      STOP 1
    endif
  endsubroutine
#endif

#ifdef __GFC_REAL_16__
  function d2rq(x)
    implicit none
    real(16), intent(in) :: x
    real(16) :: d2rq
    d2rq = (x * pi_q) / 180.0_16
  endfunction

  subroutine cmpq(x, f1, f2, tolerance, str)
    implicit none
    real(16), intent(in) :: x, f1, f2, tolerance
    character(len=*), intent(in) :: str
    if ( abs(f2 - f1) .gt. tolerance ) then
      write (*, '(A,A,F34.30,A,F34.30,F34.30)') str, "(", x, ")", f1, f2
      STOP 1
    endif
  endsubroutine
#endif

end module

use dec_math

implicit none

! inputs
real(4) :: f_i1, f_i2
real(4), volatile :: xf
real(8) :: d_i1, d_i2
real(8), volatile :: xd
#ifdef __GFC_REAL_10__
real(10) :: l_i1, l_i2
real(10), volatile :: xl
#endif
#ifdef __GFC_REAL_16__
real(16) :: q_i1, q_i2
real(16), volatile :: xq
#endif

! expected outputs from (oe) default (oxe) expression
real(4) :: f_oe, f_oxe
real(8) :: d_oe, d_oxe
#ifdef __GFC_REAL_10__
real(10) :: l_oe, l_oxe
#endif
#ifdef __GFC_REAL_16__
real(16) :: q_oe, q_oxe
#endif

! actual outputs from (oa) default (oc) constant (ox) expression
real(4) :: f_oa, f_oc, f_ox
real(8) :: d_oa, d_oc, d_ox
#ifdef __GFC_REAL_10__
real(10) :: l_oa, l_oc, l_ox
#endif
#ifdef __GFC_REAL_16__
real(16) :: q_oa, q_oc, q_ox
#endif

! tolerance of the answer: assert |exp-act| <= tol
! accept loss of ~four decimal places
real(4), parameter :: f_tol  = 5e-3_4
real(8), parameter :: d_tol  = 5e-10_8
#ifdef __GFC_REAL_10__
real(10), parameter :: l_tol = 5e-15_10
#endif
#ifdef __GFC_REAL_16__
real(16), parameter :: q_tol = 5e-20_16
#endif

! volatile multiplication factors to test non-constant expressions
xf = 2.0_4
xd = 2.0_8
#ifdef __GFC_REAL_10__
xl = 2.0_10
#endif
#ifdef __GFC_REAL_16__
xq = 2.0_16
#endif

! Input -- cos(pi/4)
f_i1 = 0.707107_4
d_i1 = 0.707106781186548_8
#ifdef __GFC_REAL_10__
l_i1 = 0.707106781186547573_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 0.707106781186547572737310929369414_16
#endif

! Expected -- pi/4
f_oe =  r2d_f * acos (f_i1)
f_oxe = r2d_f * acos (xf * f_i1)
d_oe =  r2d_d * acos (d_i1)
d_oxe = r2d_d * acos (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oe =  r2d_l * acos (l_i1)
l_oxe = r2d_l * acos (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oe =  r2d_q * acos (q_i1)
q_oxe = r2d_q * acos (xq * q_i1)
#endif

! Actual
f_oa =    acosd (f_i1)
f_oc =    acosd (0.707107_4)
f_ox = acosd (xf * f_i1)
d_oa =     acosd (d_i1)
d_oc =     acosd (0.707106781186548_8)
d_ox = acosd (xd * 0.707106781186548_8)
#ifdef __GFC_REAL_10__
l_oa =    acosd (l_i1)
l_oc =    acosd (0.707106781186547573_10)
l_ox = acosd (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa =    acosd (q_i1)
q_oc =    acosd (0.707106781186547572737310929369414_16)
q_ox = acosd (xq * 0.707106781186547572737310929369414_16)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) facosd")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) facosd")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) facosd")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dacosd")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dacosd")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) dacosd")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) lacosd")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) lacosd")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) lacosd")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qacosd")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qacosd")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qacosd")
#endif

! Input
f_i1 = 60.0_4
d_i1 = 60.0_8
#ifdef __GFC_REAL_10__
l_i1 = 60.0_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 60.0_16
#endif

! Expected
f_oe  = cos (d2rf(f_i1))
f_oxe = cos (d2rf(xf * f_i1))
d_oe  = cos (d2rd(d_i1))
d_oxe = cos (d2rd(xd * d_i1))
#ifdef __GFC_REAL_10__
l_oe  = cos (d2rl(l_i1))
l_oxe = cos (d2rl(xl * l_i1))
#endif
#ifdef __GFC_REAL_16__
q_oe  = cos (d2rq(q_i1))
q_oxe = cos (d2rq(xq * q_i1))
#endif

! Actual
f_oa = cosd (f_i1)
f_oc = cosd (60.0_4)
f_ox = cosd (xf * f_i1)
d_oa = cosd (d_i1)
d_oc = cosd (60.0_8)
d_ox = cosd (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = cosd (l_i1)
l_oc = cosd (60.0_10)
l_ox = cosd (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = cosd (q_i1)
q_oc = cosd (60.0_16)
q_ox = cosd (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fcosd")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fcosd")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fcosd")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dcosd")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dcosd")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) cosd")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) lcosd")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) lcosd")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) lcosd")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qcosd")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qcosd")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qcosd")
#endif

! Input -- sin(pi/4)
f_i1 = 0.707107_4
d_i1 = 0.707106781186548_8
#ifdef __GFC_REAL_10__
l_i1 = 0.707106781186547573_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 0.707106781186547572737310929369414_16
#endif

! Expected -- pi/4
f_oe  = r2d_f * asin (f_i1)
f_oxe = r2d_f * asin (xf * f_i1)
d_oe  = r2d_d * asin (d_i1)
d_oxe = r2d_d * asin (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oe  = r2d_l * asin (l_i1)
l_oxe = r2d_l * asin (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oe  = r2d_q * asin (q_i1)
q_oxe = r2d_q * asin (xq * q_i1)
#endif

! Actual
f_oa = asind (f_i1)
f_oc = asind (0.707107_4)
f_ox = asind (xf * f_i1)
d_oa = asind (d_i1)
d_oc = asind (0.707106781186548_8)
d_ox = asind (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = asind (l_i1)
l_oc = asind (0.707106781186547573_10)
l_ox = asind (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = asind (q_i1)
q_oc = asind (0.707106781186547572737310929369414_16)
q_ox = asind (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fasind")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fasind")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fasind")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dasind")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dasind")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) asind")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) lasind")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) lasind")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) lasind")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qasind")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qasind")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qasind")
#endif

! Input
f_i1 = 60.0_4
d_i1 = 60.0_8
#ifdef __GFC_REAL_10__
l_i1 = 60.0_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 60.0_16
#endif

! Expected
f_oe  = sin (d2rf(f_i1))
f_oxe = sin (d2rf(xf * f_i1))
d_oe  = sin (d2rd(d_i1))
d_oxe = sin (d2rd(xd * d_i1))
#ifdef __GFC_REAL_10__
l_oe  = sin (d2rl(l_i1))
l_oxe = sin (d2rl(xl * l_i1))
#endif
#ifdef __GFC_REAL_16__
q_oe  = sin (d2rq(q_i1))
q_oxe = sin (d2rq(xq * q_i1))
#endif

! Actual
f_oa = sind (f_i1)
f_oc = sind (60.0_4)
f_ox = sind (xf * f_i1)
d_oa = sind (d_i1)
d_oc = sind (60.0_8)
d_ox = sind (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = sind (l_i1)
l_oc = sind (60.0_10)
l_ox = sind (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = sind (q_i1)
q_oc = sind (60.0_16)
q_ox = sind (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fsind")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fsind")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fsind")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dsind")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dsind")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) sind")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) lsind")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) lsind")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) lsind")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qsind")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qsind")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qsind")
#endif

! Input
f_i1 = 1.0_4
f_i2 = 2.0_4
d_i1 = 1.0_8
d_i2 = 2.0_8
#ifdef __GFC_REAL_10__
l_i1 = 1.0_10
l_i2 = 2.0_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 1.0_16
q_i2 = 2.0_16
#endif

! Expected
f_oe  = r2d_f * atan2 (f_i1, f_i2)
f_oxe = r2d_f * atan2 (xf * f_i1, f_i2)
d_oe  = r2d_d * atan2 (d_i1, d_i2)
d_oxe = r2d_d * atan2 (xd * d_i1, d_i2)
#ifdef __GFC_REAL_10__
l_oe  = r2d_l * atan2 (l_i1, l_i2)
l_oxe = r2d_l * atan2 (xl * l_i1, l_i2)
#endif
#ifdef __GFC_REAL_16__
q_oe  = r2d_q * atan2 (q_i1, q_i2)
q_oxe = r2d_q * atan2 (xq * q_i1, q_i2)
#endif

! Actual
f_oa = atan2d (f_i1, f_i2)
f_oc = atan2d (1.0_4, 2.0_4)
f_ox = atan2d (xf * f_i1, f_i2)
d_oa = atan2d (d_i1, d_i2)
d_oc = atan2d (1.0_8, 2.0_8)
d_ox = atan2d (xd * d_i1, d_i2)
#ifdef __GFC_REAL_10__
l_oa = atan2d (l_i1, l_i2)
l_oc = atan2d (1.0_10, 2.0_10)
l_ox = atan2d (xl * l_i1, l_i2)
#endif
#ifdef __GFC_REAL_16__
q_oa = atan2d (q_i1, q_i2)
q_oc = atan2d (1.0_16, 2.0_16)
q_ox = atan2d (xq * q_i1, q_i2)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fatan2d")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fatan2d")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fatan2d")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) datan2d")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) datan2d")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) atan2d")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) latan2d")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) latan2d")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) latan2d")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qatan2d")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qatan2d")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qatan2d")
#endif

! Input
f_i1 = 1.55741_4
d_i1 = 1.5574077246549_8
#ifdef __GFC_REAL_10__
l_i1 = 1.55740772465490229_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 1.55740772465490229237161656783428_16
#endif

! Expected
f_oe  = r2d_f * atan (f_i1)
f_oxe = r2d_f * atan (xf * f_i1)
d_oe  = r2d_d * atan (d_i1)
d_oxe = r2d_d * atan (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oe  = r2d_l * atan (l_i1)
l_oxe = r2d_l * atan (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oe  = r2d_q * atan (q_i1)
q_oxe = r2d_q * atan (xq * q_i1)
#endif

! Actual
f_oa = atand (f_i1)
f_oc = atand (1.55741_4)
f_ox = atand (xf * f_i1)
d_oa = atand (d_i1)
d_oc = atand (1.5574077246549_8)
d_ox = atand (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = atand (l_i1)
l_oc = atand (1.55740772465490229_10)
l_ox = atand (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = atand (q_i1)
q_oc = atand (1.55740772465490229237161656783428_16)
q_ox = atand (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fatand")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fatand")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fatand")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) datand")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) datand")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) atand")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) latand")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) latand")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) latand")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qatand")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qatand")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qatand")
#endif

! Input
f_i1 = 34.3775_4
d_i1 = 34.3774677078494_8
#ifdef __GFC_REAL_10__
l_i1 = 34.3774677078493909_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 34.3774677078493908766176900826395_16
#endif

! Expected
f_oe  = 1.0_4/tan (f_i1)
f_oxe = 1.0_4/tan (xf * f_i1)
d_oe  = 1.0_8/tan (d_i1)
d_oxe = 1.0_8/tan (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oe  = 1.0_10/tan (l_i1)
l_oxe = 1.0_10/tan (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oe  = 1.0_16/tan (q_i1)
q_oxe = 1.0_16/tan (xq * q_i1)
#endif

! Actual
f_oa = cotan (f_i1)
f_oc = cotan (34.3775_4)
f_ox = cotan (xf * f_i1)
d_oa = cotan (d_i1)
d_oc = cotan (34.3774677078494_8)
d_ox = cotan (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = cotan (l_i1)
l_oc = cotan (34.3774677078493909_10)
l_ox = cotan (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = cotan (q_i1)
q_oc = cotan (34.3774677078493908766176900826395_16)
q_ox = cotan (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fcotan")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fcotan")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fcotan")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dcotan")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dcotan")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) cotan")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) lcotan")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) lcotan")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) lcotan")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qcotan")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qcotan")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qcotan")
#endif

! Input
f_i1 = 0.6_4
d_i1 = 0.6_8
#ifdef __GFC_REAL_10__
l_i1 = 0.6_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 0.6_16
#endif

! Expected
f_oe  = cotan (d2rf(f_i1))
f_oxe = cotan (d2rf(xf * f_i1))
d_oe  = cotan (d2rd(d_i1))
d_oxe = cotan (d2rd(xd * d_i1))
#ifdef __GFC_REAL_10__
l_oe  = cotan (d2rl(l_i1))
l_oxe = cotan (d2rl(xl * l_i1))
#endif
#ifdef __GFC_REAL_16__
q_oe  = cotan (d2rq(q_i1))
q_oxe = cotan (d2rq(xq * q_i1))
#endif

! Actual
f_oa = cotand (f_i1)
f_oc = cotand (0.6_4)
f_ox = cotand (xf * f_i1)
d_oa = cotand (d_i1)
d_oc = cotand (0.6_8)
d_ox = cotand (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = cotand (l_i1)
l_oc = cotand (0.6_10)
l_ox = cotand (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = cotand (q_i1)
q_oc = cotand (0.6_16)
q_ox = cotand (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) fcotand")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) fcotand")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) fcotand")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dcotand")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dcotand")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) cotand")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) lcotand")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) lcotand")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) lcotand")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qcotand")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qcotand")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qcotand")
#endif

! Input
f_i1 = 60.0_4
d_i1 = 60.0_8
#ifdef __GFC_REAL_10__
l_i1 = 60.0_10
#endif
#ifdef __GFC_REAL_16__
q_i1 = 60.0_16
#endif

! Expected
f_oe  = tan (d2rf(f_i1))
f_oxe = tan (d2rf(xf * f_i1))
d_oe  = tan (d2rd(d_i1))
d_oxe = tan (d2rd(xd * d_i1))
#ifdef __GFC_REAL_10__
l_oe  = tan (d2rl(l_i1))
l_oxe = tan (d2rl(xl * l_i1))
#endif
#ifdef __GFC_REAL_16__
q_oe  = tan (d2rq(q_i1))
q_oxe = tan (d2rq(xq * q_i1))
#endif

! Actual
f_oa = tand (f_i1)
f_oc = tand (60.0_4)
f_ox = tand (xf * f_i1)
d_oa = tand (d_i1)
d_oc = tand (60.0_8)
d_ox = tand (xd * d_i1)
#ifdef __GFC_REAL_10__
l_oa = tand (l_i1)
l_oc = tand (60.0_10)
l_ox = tand (xl * l_i1)
#endif
#ifdef __GFC_REAL_16__
q_oa = tand (q_i1)
q_oc = tand (60.0_16)
q_ox = tand (xq * q_i1)
#endif

call cmpf(f_i1, f_oe,  f_oa, f_tol, "( ) ftand")
call cmpf(f_i1, f_oe,  f_oc, f_tol, "(c) ftand")
call cmpf(f_i1, f_oxe, f_ox, f_tol, "(x) ftand")
call cmpd(d_i1, d_oe,  d_oa, d_tol, "( ) dtand")
call cmpd(d_i1, d_oe,  d_oc, d_tol, "(c) dtand")
call cmpd(d_i1, d_oxe, d_ox, d_tol, "(x) dtand")
#ifdef __GFC_REAL_10__
call cmpl(l_i1, l_oe,  l_oa, l_tol, "( ) ltand")
call cmpl(l_i1, l_oe,  l_oc, l_tol, "(c) ltand")
call cmpl(l_i1, l_oxe, l_ox, l_tol, "(x) ltand")
#endif
#ifdef __GFC_REAL_16__
call cmpq(q_i1, q_oe,  q_oa, q_tol, "( ) qtand")
call cmpq(q_i1, q_oe,  q_oc, q_tol, "(c) qtand")
call cmpq(q_i1, q_oxe, q_ox, q_tol, "(x) qtand")
#endif

end
