! { dg-options "-fdec-math" }
! { dg-do run }
!
! Test extra math intrinsics offered by -fdec-math.
!

  subroutine cmpf(f1, f2, tolerance, str)
    implicit none
    real(4), intent(in) :: f1, f2, tolerance
    character(len=*), intent(in) :: str
    if ( abs(f2 - f1) .gt. tolerance ) then
      write (*, '(A,F12.6,F12.6)') str, f1, f2
      call abort()
    endif
  endsubroutine

  subroutine cmpd(d1, d2, tolerance, str)
    implicit none
    real(8), intent(in) :: d1, d2, tolerance
    character(len=*), intent(in) :: str
    if ( dabs(d2 - d1) .gt. tolerance ) then
      write (*, '(A,F12.6,F12.6)') str, d1, d2
      call abort()
    endif
  endsubroutine

implicit none

  real(4), parameter :: pi_f = (4.0_4 *  atan(1.0_4))
  real(8), parameter :: pi_d = (4.0_8 * datan(1.0_8))
  real(4), parameter :: r2d_f = 180.0_4 / pi_f
  real(8), parameter :: r2d_d = 180.0_8 / pi_d
  real(4), parameter :: d2r_f = pi_f / 180.0_4
  real(8), parameter :: d2r_d = pi_d / 180.0_8

! inputs
real(4) :: f_i1, f_i2
real(4), volatile :: xf
real(8) :: d_i1, d_i2
real(8), volatile :: xd

! expected outputs from (oe) default (oxe) expression
real(4) :: f_oe, f_oxe
real(8) :: d_oe, d_oxe

! actual outputs from (oa) default (oc) constant (ox) expression
real(4) :: f_oa, f_oc, f_ox
real(8) :: d_oa, d_oc, d_ox

! tolerance of the answer: assert |exp-act| <= tol
real(4) :: f_tol
real(8) :: d_tol

! equivalence tolerance
f_tol = 5e-5_4
d_tol = 5e-6_8

! multiplication factors to test non-constant expressions
xf = 2.0_4
xd = 2.0_8

! Input
f_i1 = 0.68032123_4
d_i1 = 0.68032123_8

! Expected
f_oe =     r2d_f*acos (f_i1)
f_oxe = xf*r2d_f*acos (f_i1)
d_oe =     r2d_d*dacos(d_i1)
d_oxe = xd*r2d_d*dacos(d_i1)

! Actual
f_oa =    acosd (f_i1)
f_oc =    acosd (0.68032123_4)
f_ox = xf*acosd (f_i1)
d_oa =    dacosd (d_i1)
d_oc =    dacosd (0.68032123_8)
d_ox = xd*dacosd (0.68032123_8)

call cmpf(f_oe,  f_oa, f_tol, "( ) acosd")
call cmpf(f_oe,  f_oc, f_tol, "(c) acosd")
call cmpf(f_oxe, f_ox, f_tol, "(x) acosd")
call cmpd(d_oe,  d_oa, d_tol, "( ) dacosd")
call cmpd(d_oe,  d_oc, d_tol, "(c) dacosd")
call cmpd(d_oxe, d_ox, d_tol, "(x) dacosd")

! Input
f_i1 = 60.0_4
d_i1 = 60.0_8

! Expected
f_oe  =    cos (d2r_f*f_i1)
f_oxe = xf*cos (d2r_f*f_i1)
d_oe  =    cos (d2r_d*d_i1)
d_oxe = xd*cos (d2r_d*d_i1)

! Actual
f_oa =     cosd (f_i1)
f_oc =     cosd (60.0_4)
f_ox = xf* cosd (f_i1)
d_oa =    dcosd (d_i1)
d_oc =    dcosd (60.0_8)
d_ox = xd* cosd (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) cosd")
call cmpf(f_oe,  f_oc, f_tol, "(c) cosd")
call cmpf(f_oxe, f_ox, f_tol, "(x) cosd")
call cmpd(d_oe,  d_oa, d_tol, "( ) dcosd")
call cmpd(d_oe,  d_oc, d_tol, "(c) dcosd")
call cmpd(d_oxe, d_ox, d_tol, "(x) cosd")

! Input
f_i1 = 0.79345021_4
d_i1 = 0.79345021_8

! Expected
f_oe  =    r2d_f*asin (f_i1)
f_oxe = xf*r2d_f*asin (f_i1)
d_oe  =    r2d_d*asin (d_i1)
d_oxe = xd*r2d_d*asin (d_i1)

! Actual
f_oa =     asind (f_i1)
f_oc =     asind (0.79345021_4)
f_ox = xf* asind (f_i1)
d_oa =    dasind (d_i1)
d_oc =    dasind (0.79345021_8)
d_ox = xd* asind (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) asind")
call cmpf(f_oe,  f_oc, f_tol, "(c) asind")
call cmpf(f_oxe, f_ox, f_tol, "(x) asind")
call cmpd(d_oe,  d_oa, d_tol, "( ) dasind")
call cmpd(d_oe,  d_oc, d_tol, "(c) dasind")
call cmpd(d_oxe, d_ox, d_tol, "(x) asind")

! Input
f_i1 = 60.0_4
d_i1 = 60.0_8

! Expected
f_oe  =    sin (d2r_f*f_i1)
f_oxe = xf*sin (d2r_f*f_i1)
d_oe  =    sin (d2r_d*d_i1)
d_oxe = xd*sin (d2r_d*d_i1)

! Actual
f_oa =     sind (f_i1)
f_oc =     sind (60.0_4)
f_ox = xf* sind (f_i1)
d_oa =    dsind (d_i1)
d_oc =    dsind (60.0_8)
d_ox = xd* sind (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) sind")
call cmpf(f_oe,  f_oc, f_tol, "(c) sind")
call cmpf(f_oxe, f_ox, f_tol, "(x) sind")
call cmpd(d_oe,  d_oa, d_tol, "( ) dsind")
call cmpd(d_oe,  d_oc, d_tol, "(c) dsind")
call cmpd(d_oxe, d_ox, d_tol, "(x) sind")

! Input
f_i1 = 2.679676_4
f_i2 = 1.0_4
d_i1 = 2.679676_8
d_i2 = 1.0_8

! Expected
f_oe  =    r2d_f*atan2 (f_i1, f_i2)
f_oxe = xf*r2d_f*atan2 (f_i1, f_i2)
d_oe  =    r2d_d*atan2 (d_i1, d_i2)
d_oxe = xd*r2d_d*atan2 (d_i1, d_i2)

! Actual
f_oa =     atan2d (f_i1, f_i2)
f_oc =     atan2d (2.679676_4, 1.0_4)
f_ox = xf* atan2d (f_i1, f_i2)
d_oa =    datan2d (d_i1, d_i2)
d_oc =    datan2d (2.679676_8, 1.0_8)
d_ox = xd* atan2d (d_i1, d_i2)

call cmpf(f_oe,  f_oa, f_tol, "( ) atan2d")
call cmpf(f_oe,  f_oc, f_tol, "(c) atan2d")
call cmpf(f_oxe, f_ox, f_tol, "(x) atan2d")
call cmpd(d_oe,  d_oa, d_tol, "( ) datan2d")
call cmpd(d_oe,  d_oc, d_tol, "(c) datan2d")
call cmpd(d_oxe, d_ox, d_tol, "(x) atan2d")

! Input
f_i1 = 1.5874993_4
d_i1 = 1.5874993_8

! Expected
f_oe  =    r2d_f*atan (f_i1)
f_oxe = xf*r2d_f*atan (f_i1)
d_oe  =    r2d_d*atan (d_i1)
d_oxe = xd*r2d_d*atan (d_i1)

! Actual
f_oa =     atand (f_i1)
f_oc =     atand (1.5874993_4)
f_ox = xf* atand (f_i1)
d_oa =    datand (d_i1)
d_oc =    datand (1.5874993_8)
d_ox = xd* atand (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) atand")
call cmpf(f_oe,  f_oc, f_tol, "(c) atand")
call cmpf(f_oxe, f_ox, f_tol, "(x) atand")
call cmpd(d_oe,  d_oa, d_tol, "( ) datand")
call cmpd(d_oe,  d_oc, d_tol, "(c) datand")
call cmpd(d_oxe, d_ox, d_tol, "(x) atand")

! Input
f_i1 = 0.6_4
d_i1 = 0.6_8

! Expected
f_oe  =    cotan (d2r_f*f_i1)
f_oxe = xf*cotan (d2r_f*f_i1)
d_oe  =    cotan (d2r_d*d_i1)
d_oxe = xd*cotan (d2r_d*d_i1)

! Actual
f_oa =     cotand (f_i1)
f_oc =     cotand (0.6_4)
f_ox = xf* cotand (f_i1)
d_oa =    dcotand (d_i1)
d_oc =    dcotand (0.6_8)
d_ox = xd* cotand (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) cotand")
call cmpf(f_oe,  f_oc, f_tol, "(c) cotand")
call cmpf(f_oxe, f_ox, f_tol, "(x) cotand")
call cmpd(d_oe,  d_oa, d_tol, "( ) dcotand")
call cmpd(d_oe,  d_oc, d_tol, "(c) dcotand")
call cmpd(d_oxe, d_ox, d_tol, "(x) cotand")

! Input
f_i1 = 0.6_4
d_i1 = 0.6_8

! Expected
f_oe  =     1.0_4/tan (f_i1)
f_oxe = xf* 1.0_4/tan (f_i1)
d_oe  =    1.0_8/dtan (d_i1)
d_oxe = xd*1.0_8/dtan (d_i1)

! Actual
f_oa =     cotan (f_i1)
f_oc =     cotan (0.6_4)
f_ox = xf* cotan (f_i1)
d_oa =    dcotan (d_i1)
d_oc =    dcotan (0.6_8)
d_ox = xd* cotan (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) cotan")
call cmpf(f_oe,  f_oc, f_tol, "(c) cotan")
call cmpf(f_oxe, f_ox, f_tol, "(x) cotan")
call cmpd(d_oe,  d_oa, d_tol, "( ) dcotan")
call cmpd(d_oe,  d_oc, d_tol, "(c) dcotan")
call cmpd(d_oxe, d_ox, d_tol, "(x) cotan")

! Input
f_i1 = 60.0_4
d_i1 = 60.0_8

! Expected
f_oe  =    tan (d2r_f*f_i1)
f_oxe = xf*tan (d2r_f*f_i1)
d_oe  =    tan (d2r_d*d_i1)
d_oxe = xd*tan (d2r_d*d_i1)

! Actual
f_oa =     tand (f_i1)
f_oc =     tand (60.0_4)
f_ox = xf* tand (f_i1)
d_oa =    dtand (d_i1)
d_oc =    dtand (60.0_8)
d_ox = xd* tand (d_i1)

call cmpf(f_oe,  f_oa, f_tol, "( ) tand")
call cmpf(f_oe,  f_oc, f_tol, "(c) tand")
call cmpf(f_oxe, f_ox, f_tol, "(x) tand")
call cmpd(d_oe,  d_oa, d_tol, "( ) dtand")
call cmpd(d_oe,  d_oc, d_tol, "(c) dtand")
call cmpd(d_oxe, d_ox, d_tol, "(x) tand")

end
