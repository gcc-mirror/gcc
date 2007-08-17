! { dg-do compile }
! { dg-options "-fmodule-private" }
module bar
  implicit none
  public :: i
  integer :: i
end module bar

module foo
  implicit none
  integer :: j
end module foo

program main
  use bar, only : i
  use foo, only : j ! { dg-error "not found in module" }
  i = 1
  j = 1
  print *, i, j
end program main

! { dg-final { cleanup-modules "bar foo" } }
