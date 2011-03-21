! { dg-do run }
!
! PR 47455: [4.6 Regression][OOP] internal compiler error: in fold_convert_loc, at fold-const.c:2028
!
! Contributed by Thomas Henlich <thenlich@users.sourceforge.net>

module class_t
  type :: tx
    integer :: i
  end type
  type :: t
    type(tx) :: x
    procedure(find_x), pointer :: ppc
  contains
    procedure :: find_x
  end type
  type(tx), target :: zero = tx(0)
contains
  function find_x(this)
    class(t), intent(in) :: this
    type(tx), pointer :: find_x
    find_x => zero
  end function find_x
end module

program test
  use class_t
  class(t),allocatable :: this
  procedure(find_x), pointer :: pp
  allocate(this)
  ! (1) ordinary function call
  zero = tx(1)
  this%x = find_x(this)
  if (this%x%i /= 1) call abort()
  ! (2) procedure pointer
  zero = tx(2)
  pp => find_x
  this%x = pp(this)
  if (this%x%i /= 2) call abort()
  ! (3) PPC
  zero = tx(3)
  this%ppc => find_x
  this%x = this%ppc()
  if (this%x%i /= 3) call abort()
   ! (4) TBP
  zero = tx(4)
  this%x = this%find_x()
  if (this%x%i /= 4) call abort()
end

! { dg-final { cleanup-modules "class_t" } }
