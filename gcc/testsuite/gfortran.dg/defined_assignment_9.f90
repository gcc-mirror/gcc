! { dg-do run }
!
! PR fortran/57697
!
! Further test of typebound defined assignment
!
module m0
  implicit none
  type component
    integer :: i = 42
  contains
    procedure :: assign0
    generic :: assignment(=) => assign0
  end type
  type parent
    type(component) :: foo
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
  block
    type(parent), allocatable :: left
    type(parent) :: right
!    print *, right%foo
    left = right
!    print *, left%foo
    if (left%foo%i /= 20) call abort()
  end block
  block
    type(parent), allocatable :: left(:)
    type(parent) :: right(5)
!    print *, right%foo
    left = right
!    print *, left%foo
    if (any (left%foo%i /= 20)) call abort()
  end block
end
