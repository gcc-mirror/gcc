! { dg-do run }
! Test the fix for PR46897. defined_assignment_1.f90 checks that the PR
! testcases run correctly, this checks that other requirements of the
! standard are satisfied.
!
module m0
  implicit none
  type component
    integer :: i = 0
    integer, allocatable :: j(:)
  contains
    procedure :: assign0
    generic :: assignment(=)=>assign0
  end type
  type parent
    type(component) :: foo1
  end type
  type, extends(parent) :: child
    integer :: k = 1000
    integer, allocatable :: l(:)
    type(component) :: foo2
  end type
contains
  subroutine assign0(lhs,rhs)
    class(component), intent(inout) :: lhs
    class(component), intent(in) :: rhs
    if (lhs%i .eq. 0) then
      lhs%i = rhs%i
      lhs%j = rhs%j
    else
      lhs%i = rhs%i*2
      lhs%j = [rhs%j, rhs%j*2]
    end if
  end subroutine
  type(child) function new_child()
    new_child%parent%foo1%i = 20
    new_child%foo2%i = 21
    new_child%parent%foo1%j = [99,199]
    new_child%foo2%j = [199,299]
    new_child%l = [299,399]
    new_child%k = 1001
  end function
end module

program main
  use m0
  implicit none
  type(child) :: infant0

! Check that the INTENT(INOUT) of assign0 is respected and that the
! correct thing is done with allocatable components.
  infant0 = new_child()
  if (infant0%parent%foo1%i .ne. 20) call abort
  if (infant0%foo2%i .ne. 21) call abort
  if (any (infant0%parent%foo1%j .ne. [99,199])) call abort
  if (any (infant0%foo2%j .ne. [199,299])) call abort
  if (infant0%foo2%i .ne. 21) call abort
  if (any (infant0%l .ne. [299,399])) call abort

! Now, since the defined assignment depends on whether or not the 'i'
! component is the default initialization value, the result will be
! different.
  infant0 = new_child()
  if (infant0%parent%foo1%i .ne. 40) call abort
  if (any (infant0%parent%foo1%j .ne. [99,199,198,398])) call abort
  if (any (infant0%foo2%j .ne. [199,299,398,598])) call abort
  if (infant0%foo2%i .ne. 42) call abort
  if (any (infant0%l .ne. [299,399])) call abort

! Finally, make sure that normal components of the declared type survive.
  if (infant0%k .ne. 1001) call abort
end


