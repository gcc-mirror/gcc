! { dg-do run }
!
! PR fortran/57697
!
! Further test of typebound defined assignment
!
module m0
  implicit none
  type :: component
    integer :: i = 42
    integer, allocatable :: b
  contains
    procedure :: assign0
    generic :: assignment(=) => assign0
  end type
  type, extends(component) :: comp2
    real :: aa
  end type comp2
  type parent
    type(component) :: foo
    real :: cc
  end type
  type p2
    type(parent) :: x
  end type p2
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
  type(p2), allocatable :: left
  type(p2) :: right
!  print *, right%x%foo%i
  left = right
!  print *, left%x%foo%i
  if (left%x%foo%i /= 20) STOP 1
end
