! { dg-do link }
!
! PR fortran/33197
!
! Fortran complex trigonometric functions: acos, asin, atan, acosh, asinh, atanh
!
! Compile-time simplifications
!
implicit none
real(4), parameter :: pi  = 2*acos(0.0_4)
real(8), parameter :: pi8 = 2*acos(0.0_8)
real(4), parameter :: eps  = 10*epsilon(0.0_4)
real(8), parameter :: eps8 = 10*epsilon(0.0_8)
complex(4), parameter :: z0_0  = cmplx(0.0_4, 0.0_4, kind=4)
complex(4), parameter :: z1_1  = cmplx(1.0_4, 1.0_4, kind=4)
complex(8), parameter :: z80_0 = cmplx(0.0_8, 0.0_8, kind=8)
complex(8), parameter :: z81_1 = cmplx(1.0_8, 1.0_8, kind=8)

if (abs(acos(z0_0)  - cmplx(pi/2,-0.0,4)) > eps) call link_error()
if (abs(acos(z1_1)  - cmplx(0.904556894, -1.06127506,4)) > eps) call link_error()
if (abs(acos(z80_0)  - cmplx(pi8/2,-0.0_8,8)) > eps8) call link_error()
if (abs(acos(z81_1)  - cmplx(0.90455689430238140_8, -1.0612750619050357_8,8)) > eps8) call link_error()

if (abs(asin(z0_0)  - cmplx(0.0,0.0,4)) > eps) call link_error()
if (abs(asin(z1_1)  - cmplx(0.66623943, 1.06127506,4)) > eps) call link_error()
if (abs(asin(z80_0)  - cmplx(0.0_8,0.0_8,8)) > eps8) call link_error()
if (abs(asin(z81_1)  - cmplx(0.66623943249251527_8, 1.0612750619050357_8,8)) > eps8) call link_error()

if (abs(atan(z0_0)  - cmplx(0.0,0.0,4)) > eps) call link_error()
if (abs(atan(z1_1)  - cmplx(1.01722196, 0.40235947,4)) > eps) call link_error()
if (abs(atan(z80_0)  - cmplx(0.0_8,0.0_8,8)) > eps8) call link_error()
if (abs(atan(z81_1)  - cmplx(1.0172219678978514_8, 0.40235947810852507_8,8)) > eps8) call link_error()

if (abs(acosh(z0_0)  - cmplx(0.0,pi/2,4)) > eps) call link_error()
if (abs(acosh(z1_1)  - cmplx(1.06127506, 0.90455689,4)) > eps) call link_error()
if (abs(acosh(z80_0)  - cmplx(0.0_8,pi8/2,8)) > eps8) call link_error()
if (abs(acosh(z81_1)  - cmplx(1.0612750619050357_8, 0.90455689430238140_8,8)) > eps8) call link_error()

if (abs(asinh(z0_0)  - cmplx(0.0,0.0,4)) > eps) call link_error()
if (abs(asinh(z1_1)  - cmplx(1.06127506, 0.66623943,4)) > eps) call link_error()
if (abs(asinh(z80_0)  - cmplx(0.0_8,0.0_8,8)) > eps8) call link_error()
if (abs(asinh(z81_1)  - cmplx(1.0612750619050357_8, 0.66623943249251527_8,8)) > eps8) call link_error()

if (abs(atanh(z0_0)  - cmplx(0.0,0.0,4)) > eps) call link_error()
if (abs(atanh(z1_1)  - cmplx(0.40235947, 1.01722196,4)) > eps) call link_error()
if (abs(atanh(z80_0)  - cmplx(0.0_8,0.0_8,8)) > eps8) call link_error()
if (abs(atanh(z81_1)  - cmplx(0.40235947810852507_8, 1.0172219678978514_8,8)) > eps8) call link_error()

end
