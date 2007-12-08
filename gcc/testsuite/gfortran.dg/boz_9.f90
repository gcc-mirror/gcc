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
real,parameter             :: rc  = real(z'3333')
double precision,parameter :: dc  = dble(Z'3FD34413509F79FF')
complex,parameter          :: z1c = cmplx(b'10101',-4.0)
complex,parameter          :: z2c = cmplx(5.0, o'01245')

real             :: r2 = real(int(z'3333'))
real             :: r  = real(z'3333')
double precision :: d  = dble(Z'3FD34413509F79FF')
complex          :: z1 = cmplx(b'10101',-4.0)
complex          :: z2 = cmplx(5.0, o'01245')

if (r2c /= 13107.0) stop '1'
if (rc  /= 1.83668190E-41) stop '2'
if (dc /= 0.30102999566398120) stop '3'
if (real(z1c) /= 2.94272678E-44 .or. aimag(z1c) /= -4.0) stop '4'
if (real(z2c) /= 5.0 .or. aimag(z2c) /= 9.48679060E-43) stop '5'

if (r2 /= 13107.0) stop '1'
if (r  /= 1.83668190E-41) stop '2'
if (d /= 0.30102999566398120) stop '3'
if (real(z1) /= 2.94272678E-44 .or. aimag(z1) /= -4.0) stop '4'
if (real(z2) /= 5.0 .or. aimag(z2) /= 9.48679060E-43) stop '5'

r2 = dble(int(z'3333'))
r = real(z'3333')
d = dble(Z'3FD34413509F79FF')
z1 = cmplx(b'10101',-4.0)
z2 = cmplx(5.0, o'01245')

if (r2 /= 13107.0) stop '1'
if (r  /= 1.83668190E-41) stop '2'
if (d /= 0.30102999566398120) stop '3'
if (real(z1) /= 2.94272678E-44 .or. aimag(z1) /= -4.0) stop '4'
if (real(z2) /= 5.0 .or. aimag(z2) /= 9.48679060E-43) stop '5'

call test4()
call test8()

contains

subroutine test4
real,parameter             :: r2c = real(int(z'3333', kind=4), kind=4)
real,parameter             :: rc  = real(z'3333', kind=4)
complex,parameter          :: z1c = cmplx(b'10101',-4.0, kind=4)
complex,parameter          :: z2c = cmplx(5.0, o'01245', kind=4)

real             :: r2 = real(int(z'3333', kind=4), kind=4)
real             :: r  = real(z'3333', kind=4)
complex          :: z1 = cmplx(b'10101',-4.0, kind=4)
complex          :: z2 = cmplx(5.0, o'01245', kind=4)

if (r2c /= 13107.0) stop '1'
if (rc  /= 1.83668190E-41) stop '2'
if (real(z1c) /= 2.94272678E-44 .or. aimag(z1c) /= -4.0) stop '4'
if (real(z2c) /= 5.0 .or. aimag(z2c) /= 9.48679060E-43) stop '5'

if (r2 /= 13107.0) stop '1'
if (r  /= 1.83668190E-41) stop '2'
if (real(z1) /= 2.94272678E-44 .or. aimag(z1) /= -4.0) stop '4'
if (real(z2) /= 5.0 .or. aimag(z2) /= 9.48679060E-43) stop '5'

r2 = real(int(z'3333'), kind=4)
r = real(z'3333', kind=4)
z1 = cmplx(b'10101',-4.0, kind=4)
z2 = cmplx(5.0, o'01245', kind=4)

if (r2 /= 13107.0) stop '1'
if (r  /= 1.83668190E-41) stop '2'
if (real(z1) /= 2.94272678E-44 .or. aimag(z1) /= -4.0) stop '4'
if (real(z2) /= 5.0 .or. aimag(z2) /= 9.48679060E-43) stop '5'
end subroutine test4


subroutine test8
real(8),parameter     :: r2c = real(int(z'FFFFFF3333', kind=8), kind=8)
real(8),parameter     :: rc  = real(z'AAAAAAAAAAAAAAAAFFFFFFF3333', kind=8)
complex(8),parameter  :: z1c = cmplx(b'11111011111111111111111111111111111111111111111111111111110101',-4.0, kind=8)
complex(8),parameter  :: z2c = cmplx(5.0, o'444444444442222222222233301245', kind=8)

real(8)             :: r2 = real(int(z'FFFFFF3333',kind=8),kind=8)
real(8)             :: r  = real(z'AAAAAAAAAAAAAAAAFFFFFFF3333', kind=8)
complex(8)          :: z1 = cmplx(b'11111011111111111111111111111111111111111111111111111111110101',-4.0, kind=8)
complex(8)          :: z2 = cmplx(5.0, o'444444444442222222222233301245', kind=8)

if (r2c /= 1099511575347.0d0) stop '1'
if (rc  /= -3.72356884822177915d-103) stop '2'
if (real(z1c) /= 3.05175781249999627d-5 .or. aimag(z1c) /= -4.0) stop '4'
if (real(z2c) /= 5.0 .or. aimag(z2c) /= 3.98227593015308981d41) stop '5'

if (r2 /= 1099511575347.0d0) stop '1'
if (r  /= -3.72356884822177915d-103) stop '2'
if (real(z1) /= 3.05175781249999627d-5 .or. aimag(z1) /= -4.0) stop '4'
if (real(z2) /= 5.0 .or. aimag(z2) /= 3.98227593015308981d41) stop '5'

r2 = real(int(z'FFFFFF3333',kind=8),kind=8)
r  = real(z'AAAAAAAAAAAAAAAAFFFFFFF3333', kind=8)
z1 = cmplx(b'11111011111111111111111111111111111111111111111111111111110101',-4.0, kind=8)
z2 = cmplx(5.0, o'444444444442222222222233301245', kind=8)

if (r2 /= 1099511575347.0d0) stop '1'
if (r  /= -3.72356884822177915d-103) stop '2'
if (real(z1) /= 3.05175781249999627d-5 .or. aimag(z1) /= -4.0) stop '4'
if (real(z2) /= 5.0 .or. aimag(z2) /= 3.98227593015308981d41) stop '5'

end subroutine test8

end program f2003
