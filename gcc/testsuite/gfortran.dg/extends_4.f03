! { dg-do run }
! Check that derived type extension is compatible with renaming
! the parent type and that allocatable components are OK.  At
! the same time, private type and components are checked.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module mymod
  type :: a
    real, allocatable :: x(:)
    integer, private :: ia = 0
  end type a
  type :: b
    private
    real, allocatable :: x(:)
    integer :: i
  end type b
contains
  function set_b () result (res)
    type(b) :: res
    allocate (res%x(2))
    res%x = [10.0, 20.0]
    res%i = 1
  end function
  subroutine check_b (arg)
    type(b) :: arg
    if (any (arg%x /= [10.0, 20.0])) call abort
    if (arg%i /= 1) call abort
  end subroutine
end module mymod

  use mymod, e => a
  type, extends(e) :: f
    integer :: if
  end type f
  type, extends(b) :: d
    integer :: id
  end type d

  type(f) :: p
  type(d) :: q

  p = f (x = [1.0, 2.0], if = 3)
  if (any (p%e%x /= [1.0, 2.0])) call abort

  q%b = set_b ()
  call check_b (q%b)
  q = d (b = set_b (), id = 99)
  call check_b (q%b)
end

! { dg-final { cleanup-modules "mymod" } }
