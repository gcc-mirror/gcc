! { dg-do run }
!
! Test that pr78395 is fixed.
! Contributed by Chris MacMackin and Janus Weil

module types_mod
  implicit none

  type, public :: t1
    integer :: a
  contains
    procedure :: get_t2
  end type

  type, public :: t2
    integer :: b
  contains
    procedure, pass(rhs) :: mul2
    procedure :: assign
    generic :: operator(*) => mul2
    generic :: assignment(=) => assign
  end type

contains

  function get_t2(this)
    class(t1), intent(in) :: this
    class(t2), allocatable :: get_t2
    type(t2), allocatable :: local
    allocate(local)
    local%b = this%a
    call move_alloc(local, get_t2)
  end function

  function mul2(lhs, rhs)
    class(t2), intent(in) :: rhs
    integer, intent(in) :: lhs
    class(t2), allocatable :: mul2
    type(t2), allocatable :: local
    allocate(local)
    local%b = rhs%b*lhs
    call move_alloc(local, mul2)
  end function

  subroutine assign(this, rhs)
    class(t2), intent(out) :: this
    class(t2), intent(in)  :: rhs
    select type(rhs)
    type is(t2)
      this%b = rhs%b
    class default
      error stop
    end select
  end subroutine

end module


program minimal
  use types_mod
  implicit none

  class(t1), allocatable :: v4
  class(t2), allocatable :: v6

  allocate(v4, source=t1(4))
  allocate(v6)
  v6 = 3 * v4%get_t2() 

  select type (v6)
    type is (t2)
      if (v6%b /= 12) error stop
    class default
      error stop
  end select
  deallocate(v4, v6)
end

