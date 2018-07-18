! { dg-do run }
! Further test of typebound defined assignment
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
    class(component), intent(INout) :: lhs
    class(component), intent(in) :: rhs
    lhs%i = 20
  end subroutine
end module

module m1
  implicit none
  type component1
    integer :: i = 0
  contains
    procedure :: assign1
    generic :: assignment(=)=>assign1
  end type
  type parent1
    type(component1) :: foo
  end type
  type, extends(parent1) :: child1
    integer :: j = 7
  end type
contains
  impure elemental subroutine assign1(lhs,rhs)
    class(component1), intent(out) :: lhs
    class(component1), intent(in) :: rhs
    lhs%i = 30
  end subroutine
end module


program main
  use m0
  use m1
  implicit none
  type(child) :: infant(2)
  type(parent) :: dad, mum
  type(child1) :: orphan(5)
  type(child1), allocatable :: annie(:)
  integer :: i, j, k

  dad = parent ([component (3), component (4)])
  mum = parent ([component (5), component (6)])
  infant = [child(dad, 999), child(mum, 9999)]  ! { dg-warning "multiple part array references" }

! Check that array sections are OK
  i = 3
  j = 4
  orphan(i:j) = child1(component1(777), 1)
  if (any (orphan%parent1%foo%i .ne. [0,0,30,30,0])) STOP 1
  if (any (orphan%j .ne. [7,7,1,1,7])) STOP 2

! Check that allocatable lhs's work OK.
  annie = [(child1(component1(k), 2*k), k = 1,3)]
  if (any (annie%parent1%foo%i .ne. [30,30,30])) STOP 3
  if (any (annie%j .ne. [2,4,6])) STOP 4
end


