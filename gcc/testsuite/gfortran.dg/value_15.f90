! { dg-do run }
! PR 49802
! Sequence association (F2023, 15.5.2.12) of a scalar actual argument
! with an explicit-shape array dummy that has the VALUE attribute used
! to ICE in conv_dummy_value.  The dummy receives a private copy of as
! many elements as it declares.

program test
  implicit none
  type :: dt
    integer, allocatable :: d(:)
  end type
  integer :: a(20), i
  character(len=12) :: s
  type(dt) :: t(3)

  a = [(i, i=1,20)]
  s = "abcdefghijkl"

  call const_bound (a(3))
  if (any (a /= [(i, i=1,20)])) stop 1

  call dummy_bound (4, a(3))
  if (any (a /= [(i, i=1,20)])) stop 2

  call rank_two (a(5))
  if (any (a /= [(i, i=1,20)])) stop 3

  call char_elems (s)
  if (s /= "abcdefghijkl") stop 4

  call char_dummy_len (3, s)
  if (s /= "abcdefghijkl") stop 5

  do i = 1, 3
    allocate (t(i)%d(2), source=[i,i])
  end do
  call alloc_comp (t(1))
  if (any (t(1)%d /= [1,1])) stop 6
  if (any (t(2)%d /= [2,2])) stop 7

contains

  subroutine const_bound (x)
    integer, value :: x(5)
    if (any (x /= [3,4,5,6,7])) stop 11
    x = -1
  end subroutine

  subroutine dummy_bound (n, x)
    integer, intent(in) :: n
    integer, value :: x(n)
    if (size (x) /= 4) stop 21
    if (any (x /= [3,4,5,6])) stop 22
    x = -1
  end subroutine

  subroutine rank_two (x)
    integer, value :: x(2,3)
    if (any (reshape (x, [6]) /= [5,6,7,8,9,10])) stop 31
    x = -1
  end subroutine

  subroutine char_elems (x)
    character(len=3), value :: x(4)
    if (x(1) /= "abc" .or. x(4) /= "jkl") stop 41
    x = "ZZZ"
  end subroutine

  ! The copy is deep: the callee must not reach the actual argument's
  ! data through a shared component pointer.
  subroutine alloc_comp (x)
    type(dt), value :: x(2)
    if (any (x(1)%d /= [1,1])) stop 61
    if (any (x(2)%d /= [2,2])) stop 62
    x(1)%d = [-1,-1]
    x(2)%d = [-9,-9]
    if (any (x(1)%d /= [-1,-1])) stop 63
    if (any (x(2)%d /= [-9,-9])) stop 64
  end subroutine

  subroutine char_dummy_len (n, x)
    integer, intent(in) :: n
    character(len=n), value :: x(4)
    if (len (x) /= 3) stop 51
    if (x(1) /= "abc" .or. x(4) /= "jkl") stop 52
    x = "ZZZ"
  end subroutine

end program
