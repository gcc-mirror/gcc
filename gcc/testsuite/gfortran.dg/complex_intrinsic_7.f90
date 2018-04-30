! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/33197
!
! Fortran 2008 complex trigonometric functions: tan, cosh, sinh, tanh
!
! Compile-time simplificiations
!
implicit none
real(4), parameter :: pi  = 2*acos(0.0_4)
real(8), parameter :: pi8 = 2*acos(0.0_8)
real(4), parameter :: eps  = 10*epsilon(0.0_4)
real(8), parameter :: eps8 = 10*epsilon(0.0_8)
complex(4), parameter :: z0_0  = cmplx(0.0_4, 0.0_4, kind=4)
complex(4), parameter :: z1_1  = cmplx(1.0_4, 1.0_4, kind=4)
complex(4), parameter :: zp_p  = cmplx(pi,    pi,    kind=4)
complex(8), parameter :: z80_0 = cmplx(0.0_8, 0.0_8, kind=8)
complex(8), parameter :: z81_1 = cmplx(1.0_8, 1.0_8, kind=8)
complex(8), parameter :: z8p_p = cmplx(pi8,   pi8,   kind=8)

if (abs(tan(z0_0)  - cmplx(0.0,0.0,4)) > eps) STOP 1
if (abs(tan(z1_1)  - cmplx(0.27175257,1.0839232,4)) > eps) STOP 2
if (abs(tan(z80_0) - cmplx(0.0_8,0.0_8,8)) > eps8) STOP 3
if (abs(tan(z81_1) - cmplx(0.27175258531951174_8,1.0839233273386946_8,8)) > eps8) STOP 4

if (abs(cosh(z0_0)  - cmplx(1.0,0.0,4)) > eps) STOP 5
if (abs(cosh(z1_1)  - cmplx(0.83372992,0.98889768,4)) > eps) STOP 6
if (abs(cosh(z80_0) - cmplx(1.0_8,0.0_8,8)) > eps8) STOP 7
if (abs(cosh(z81_1) - cmplx(0.83373002513114913_8,0.98889770576286506_8,8)) > eps8) STOP 8

if (abs(sinh(z0_0)  - cmplx(0.0,0.0,4)) > eps) STOP 9
if (abs(sinh(z1_1)  - cmplx(0.63496387,1.2984575,4)) > eps) STOP 10
if (abs(sinh(z80_0) - cmplx(0.0_8,0.0_8,8)) > eps8) STOP 11
if (abs(sinh(z81_1) - cmplx(0.63496391478473613_8,1.2984575814159773_8,8)) > eps8) STOP 12

if (abs(tanh(z0_0)  - cmplx(0.0,0.0,4)) > eps) STOP 13
if (abs(tanh(z1_1)  - cmplx(1.0839232,0.27175257,4)) > eps) STOP 14
if (abs(tanh(z80_0) - cmplx(0.0_8,0.0_8,8)) > eps8) STOP 15
if (abs(tanh(z81_1) - cmplx(1.0839233273386946_8,0.27175258531951174_8,8)) > eps8) STOP 16

end
! { dg-final { scan-tree-dump-times "_gfortran_stop" 0 "original" } }
