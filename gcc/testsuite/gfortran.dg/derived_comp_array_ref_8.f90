! { dg-do compile }
!
! PR fortran/52325
!
implicit none
real :: f
cc%a = 5 ! { dg-error "Symbol 'cc' at .1. has no IMPLICIT type" }
f%a = 5  ! { dg-error "Unexpected '%' for nonderived-type variable 'f' at" }
end
