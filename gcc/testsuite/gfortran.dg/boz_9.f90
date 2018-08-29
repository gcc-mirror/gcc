! { dg-do run }
! { dg-options "-fno-range-check" }
!
! PR fortran/34342
!
! Test for Fortran 2003 BOZ.
!
program f2003
implicit none

real,parameter             :: r2c = real(int(z'3333'))
real,parameter             :: rc  = real(z'50CB9F09')
double precision,parameter :: dc  = dble(Z'3FD34413509F79FF')
complex,parameter          :: z1c = cmplx(b'11000001010001101101110110000011', 3.049426e-10)
complex,parameter          :: z2c = cmplx(4.160326e16, o'6503667306')

real             :: r2 = real(int(z'3333'))
real             :: r  = real(z'50CB9F09')
double precision :: d  = dble(Z'3FD34413509F79FF')
complex          :: z1 = cmplx(b'11000001010001101101110110000011', 3.049426e-10)
complex          :: z2 = cmplx(4.160326e16, o'6503667306')

if (r2c /= 13107.0) STOP 1
if (rc  /= 2.732958e10) STOP 2
if (dc /= 0.30102999566398120d0) STOP 3
if (real(z1c) /= -1.242908e1 .or. aimag(z1c) /= 3.049426e-10) STOP 4
if (real(z2c) /= 4.160326e16 .or. aimag(z2c) /= 5.343285e-7) STOP 5

if (r2 /= 13107.0) STOP 6
if (r  /= 2.732958e10) STOP 7
if (d /= 0.30102999566398120d0) STOP 8
if (real(z1) /= -1.242908e1 .or. aimag(z1) /= 3.049426e-10) STOP 9
if (real(z2) /= 4.160326e16 .or. aimag(z2) /= 5.343285e-7) STOP 10

r2 = dble(int(z'3333'))
r = real(z'50CB9F09')
d = dble(Z'3FD34413509F79FF')
z1 = cmplx(b'11000001010001101101110110000011', 3.049426e-10)
z2 = cmplx(4.160326e16, o'6503667306')

if (r2 /= 13107d0) STOP 11
if (r  /= 2.732958e10) STOP 12
if (d /= 0.30102999566398120d0) STOP 13
if (real(z1) /= -1.242908e1 .or. aimag(z1) /= 3.049426e-10) STOP 14
if (real(z2) /= 4.160326e16 .or. aimag(z2) /= 5.343285e-7) STOP 15

call test4()
call test8()

contains

subroutine test4
real,parameter             :: r2c = real(int(z'3333', kind=4), kind=4)
real,parameter             :: rc  = real(z'50CB9F09', kind=4)
complex,parameter          :: z1c = cmplx(b'11000001010001101101110110000011', 3.049426e-10, kind=4)
complex,parameter          :: z2c = cmplx(4.160326e16, o'6503667306', kind=4)

real             :: r2 = real(int(z'3333', kind=4), kind=4)
real             :: r  = real(z'50CB9F09', kind=4)
complex          :: z1 = cmplx(b'11000001010001101101110110000011', 3.049426e-10, kind=4)
complex          :: z2 = cmplx(4.160326e16, o'6503667306', kind=4)

if (r2c /= 13107.0) STOP 16
if (rc  /= 2.732958e10) STOP 17
if (real(z1) /= -1.242908e1 .or. aimag(z1) /= 3.049426e-10) STOP 18
if (real(z2) /= 4.160326e16 .or. aimag(z2) /= 5.343285e-7) STOP 19

if (r2 /= 13107.0) STOP 20
if (r  /= 2.732958e10) STOP 21
if (real(z1) /= -1.242908e1 .or. aimag(z1) /= 3.049426e-10) STOP 22
if (real(z2) /= 4.160326e16 .or. aimag(z2) /= 5.343285e-7) STOP 23

r2 = real(int(z'3333'), kind=4)
r = real(z'50CB9F09', kind=4)
z1 = cmplx(b'11000001010001101101110110000011', 3.049426e-10, kind=4)
z2 = cmplx(4.160326e16, o'6503667306', kind=4)

if (r2 /= 13107.0) STOP 24
if (r  /= 2.732958e10) STOP 25
if (real(z1) /= -1.242908e1 .or. aimag(z1) /= 3.049426e-10) STOP 26
if (real(z2) /= 4.160326e16 .or. aimag(z2) /= 5.343285e-7) STOP 27
end subroutine test4


subroutine test8
real(8),parameter     :: r2c = real(int(z'FFFFFF3333', kind=8), kind=8)
real(8),parameter     :: rc  = real(z'AAAAAFFFFFFF3333', kind=8)
complex(8),parameter  :: z1c = cmplx(b'11111011111111111111111111111111111111111111111111111111110101',-4.0, kind=8)
complex(8),parameter  :: z2c = cmplx(5.0, o'442222222222233301245', kind=8)

real(8)             :: r2 = real(int(z'FFFFFF3333',kind=8),kind=8)
real(8)             :: r  = real(z'AAAAAFFFFFFF3333', kind=8)
complex(8)          :: z1 = cmplx(b'11111011111111111111111111111111111111111111111111111111110101',-4.0, kind=8)
complex(8)          :: z2 = cmplx(5.0, o'442222222222233301245', kind=8)

if (r2c /= 1099511575347.0d0) STOP 28
if (rc  /= -3.72356884822177915d-103) STOP 29
if (real(z1c) /= 3.05175781249999627d-5 .or. aimag(z1c) /= -4.0) STOP 30
if (real(z2c) /= 5.0 .or. aimag(z2c) /= 3.98227593015308981d41) STOP 31

if (r2 /= 1099511575347.0d0) STOP 32
if (r  /= -3.72356884822177915d-103) STOP 33
if (real(z1) /= 3.05175781249999627d-5 .or. aimag(z1) /= -4.0) STOP 34
if (real(z2) /= 5.0 .or. aimag(z2) /= 3.98227593015308981d41) STOP 35

r2 = real(int(z'FFFFFF3333',kind=8),kind=8)
r  = real(z'AAAAAFFFFFFF3333', kind=8)
z1 = cmplx(b'11111011111111111111111111111111111111111111111111111111110101',-4.0, kind=8)
z2 = cmplx(5.0, o'442222222222233301245', kind=8)

if (r2 /= 1099511575347.0d0) STOP 36
if (r  /= -3.72356884822177915d-103) STOP 37
if (real(z1) /= 3.05175781249999627d-5 .or. aimag(z1) /= -4.0) STOP 38
if (real(z2) /= 5.0 .or. aimag(z2) /= 3.98227593015308981d41) STOP 39

end subroutine test8

end program f2003
