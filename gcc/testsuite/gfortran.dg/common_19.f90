! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/48858
!
integer :: i
common /foo/ i
bind(C) :: /foo/ ! { dg-error "Fortran 2003: BIND.C. statement" }
end
