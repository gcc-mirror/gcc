! { dg-do compile }
!
! PR fortran/58469
!
! Related: PR fortran/57697
!
! Was ICEing before
!
module m0
  implicit none
  type :: component
    integer :: i = 42
  contains
    procedure :: assign0
    generic :: assignment(=) => assign0
  end type
  type, extends(component) :: comp2
    real :: aa
  end type comp2
  type parent
    type(comp2) :: foo
  end type
contains
  elemental subroutine assign0(lhs,rhs)
    class(component), intent(INout) :: lhs
    class(component), intent(in) :: rhs
    lhs%i = 20
  end subroutine
end module

program main
  use m0
  implicit none
  type(parent), allocatable :: left
  type(parent) :: right
  print *, right%foo
  left = right
  print *, left%foo
  if (left%foo%i /= 42) call abort()
end
