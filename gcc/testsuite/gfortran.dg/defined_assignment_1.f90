! { dg-do run }
! Test the fix for PR46897.
!
! Contributed by Rouson Damian <rouson@sandia.gov>
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
    type(component) :: foo
  end type
  type, extends(parent) :: child
    integer :: j
  end type
contains
  subroutine assign0(lhs,rhs)
    class(component), intent(out) :: lhs
    class(component), intent(in) :: rhs
    lhs%i = 20
  end subroutine 
  type(child) function new_child()
  end function
end module 

module m1
  implicit none
  type component1
    integer :: i = 1
  contains
    procedure :: assign1
    generic :: assignment(=)=>assign1
  end type
  type t
    type(component1) :: foo
  end type
contains
  subroutine assign1(lhs,rhs)
    class(component1), intent(out) :: lhs
    class(component1), intent(in) :: rhs
    lhs%i = 21
  end subroutine
end module

module m2
  implicit none
  type component2
    integer :: i = 2
  end type
  interface assignment(=)
    module procedure assign2
  end interface
  type t2
    type(component2) :: foo
  end type
contains
  subroutine assign2(lhs,rhs)
    type(component2), intent(out) :: lhs
    type(component2), intent(in) :: rhs
    lhs%i = 22
  end subroutine
end module 

program main
  use m0
  use m1
  use m2
  implicit none
  type(child) :: infant0
  type(t) :: infant1, newchild1
  type(t2) :: infant2, newchild2

! Test the reported problem.
  infant0 = new_child()
  if (infant0%parent%foo%i .ne. 20) call abort

! Test the case of comment #1 of the PR.
  infant1 = newchild1
  if (infant1%foo%i .ne. 21) call abort

! Test the case of comment #2 of the PR.
  infant2 = newchild2
  if (infant2%foo%i .ne. 2) call abort
end


