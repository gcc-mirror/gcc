! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/27997
!
! Array constructor with typespec
! should be rejected for Fortran 95.
!
real :: a(3)
integer :: j(3)
a = (/ integer :: 1.4, 2.2, 3.33  /) ! { dg-error "Fortran 2003" }
j = (/ 1.4, 2.2, 3.33  /)
if( any(a /= j )) STOP 1
end
