! { dg-do run }
! { dg-require-visibility "" }
!
! PR 47565: [4.6 Regression][OOP] Segfault with TBP
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module class_t
  type :: t
    procedure(find_y), pointer, nopass :: ppc
  contains
    procedure, nopass :: find_y
  end type
  integer, private :: count = 0
contains
  function find_y() result(res)
    integer, allocatable :: res
    allocate(res)
    count = count + 1
    res = count
  end function
end module

program p
  use class_t
  class(t), allocatable :: this
  integer :: y

  allocate(this)
  this%ppc => find_y
  ! (1) ordinary procedure
  y = find_y()
  if (y/=1) STOP 1
  ! (2) procedure pointer component
  y = this%ppc()
  if (y/=2) STOP 2
  ! (3) type-bound procedure
  y = this%find_y()
  if (y/=3) STOP 3
end 
