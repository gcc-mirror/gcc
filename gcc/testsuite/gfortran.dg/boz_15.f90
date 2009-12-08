! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! { dg-require-effective-target fortran_large_int }
!
! PR fortran/41711
!
! Check reading and writing of real(10/16) BOZ,
! which needs integer(16) support.
!
implicit none
character(len=255) :: str
integer,parameter :: xp = selected_real_kind (precision (0.0d0)+1)
real(xp)    :: r1,r2
complex(xp) :: z1,z2

r2 = 5.0_xp
r1 = 2.0_xp
! Real B(OZ)
write(str,'(b126)') r1
read (str,'(b126)') r2
if(r2 /= r1) call abort()
! Real (B)O(Z)
r2 = 5.0_xp
write(str,'(o126)') r1
read (str,'(o126)') r2
if(r2 /= r1) call abort()
! Real (BO)Z
r2 = 5.0_xp
write(str,'(z126)') r1
read (str,'(z126)') r2
if(r2 /= r1) call abort()

z2 = cmplx(5.0_xp,7.0_xp)
z1 = cmplx(2.0_xp,3.0_xp)
! Complex B(OZ)
write(str,'(2b126)') z1
read (str,'(2b126)') z2
if(z2 /= z1) call abort()
! Complex (B)O(Z)
z2 = cmplx(5.0_xp,7.0_xp)
write(str,'(2o126)') z1
read (str,'(2o126)') z2
if(z2 /= z1) call abort()
! Complex (BO)Z
z2 = cmplx(5.0_xp,7.0_xp)
write(str,'(2z126)') z1
read (str,'(2z126)') z2
if(z2 /= z1) call abort()
end
