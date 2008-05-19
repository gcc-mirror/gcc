! { dg-do compile }
! See PR fortran/36251.
module a
  implicit none
  integer :: i = 42
end module a

! Causes ICE
module b
  use iso_c_binding
  use a
  implicit none
  bind(c) :: a  ! { dg-error "applied to" }
end module b

! Causes ICE
module d
  use a
  implicit none
  bind(c) :: a  ! { dg-error "applied to" }
end module d
! { dg-final { cleanup-modules "a" } }
