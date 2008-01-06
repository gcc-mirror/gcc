! { dg-do compile }
!
! PR fortran/34658
!
! Check for more COMMON constrains
!
block data
  implicit none
  integer :: x, a  ! { dg-warning "Initialized variable 'a' at .1. is in a blank COMMON" }
  integer :: y = 5, b = 5 ! { dg-warning "Initialized variable 'b' at .1. is in a blank COMMON" }
  data x/5/, a/5/
  common // a, b
  common /a/ x, y
end block data

subroutine foo()
  implicit none
  type t
    sequence
    integer :: i = 5
  end type t
  type(t) x ! { dg-error "may not have default initializer" }
  common // x
end subroutine foo

program test
  implicit none
  common /a/ I ! { dg-warning "in COMMON but only in BLOCK DATA initialization" }
  integer :: I = 43
end program test
