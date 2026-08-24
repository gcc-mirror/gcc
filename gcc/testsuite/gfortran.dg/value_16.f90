! { dg-do run }
! PR 49802
! A VALUE array dummy of a derived type with allocatable components was
! given a shallow copy, so the callee reached the actual argument's data
! through the shared component pointers.  The copy must be deep.

program test
  implicit none
  type :: inner
    integer, allocatable :: d(:)
  end type
  type :: outer
    type(inner), allocatable :: b(:)
    character(:), allocatable :: nm
  end type
  type(outer) :: v(2)
  integer :: k

  do k = 1, 2
    allocate (v(k)%b(2))
    allocate (v(k)%b(1)%d(2), source=[k,k])
    allocate (v(k)%b(2)%d(2), source=[10*k,10*k])
    v(k)%nm = "orig"
  end do

  call explicit_shape (v)
  if (any (v(1)%b(1)%d /= [1,1])) stop 1
  if (v(1)%nm /= "orig") stop 2

  call assumed_shape (v)
  if (any (v(2)%b(2)%d /= [20,20])) stop 3
  if (v(2)%nm /= "orig") stop 4

contains

  subroutine explicit_shape (x)
    type(outer), value :: x(2)
    x(1)%b(1)%d = [-1,-1]
    x(1)%nm = "changed"
    if (any (x(1)%b(1)%d /= [-1,-1])) stop 11
  end subroutine

  subroutine assumed_shape (x)
    type(outer), value :: x(:)
    x(2)%b(2)%d = [-9,-9]
    x(2)%nm = "changed"
    if (x(2)%nm /= "changed") stop 21
  end subroutine

end program
