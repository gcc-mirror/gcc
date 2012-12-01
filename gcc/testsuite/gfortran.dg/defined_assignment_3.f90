! { dg-do run }
! Test the fix for PR46897. defined_assignment_1.f90 checks that the PR
! testcases run correctly, this checks array components are OK.
!
module m0
  implicit none
  type component
    integer :: i = 0
  contains
    procedure :: assign0
    generic :: assignment(=)=>assign0
  end type
  type parent
    type(component) :: foo(2)
  end type
  type, extends(parent) :: child
    integer :: j
  end type
contains
  elemental subroutine assign0(lhs,rhs)
    class(component), intent(out) :: lhs
    class(component), intent(in) :: rhs
    lhs%i = 20
  end subroutine
end module


program main
  use m0
  implicit none
  type(child) :: infant0, infant1(2)

  infant0 = child([component(1),component(2)], 99)
  if (any (infant0%parent%foo%i .ne. [20, 20])) call abort

end


