! { dg-do compile }

implicit none
integer :: x
!$acc enter data attach(x)
! { dg-error "'attach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }

end
