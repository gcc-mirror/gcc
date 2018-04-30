! { dg-do run }
!
! PR fortran/27997
!
! Array constructor with typespec.
!
real :: a(3)
integer :: j(3)
a = (/ integer :: 1.4, 2.2, 3.33  /)
j = (/ 1.4, 2.2, 3.33  /)
if( any(a /= j )) STOP 1
end
