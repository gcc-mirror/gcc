! { dg-do run }
! PR53800

! A CLASS array actual passed to an assumed-shape TYPE dummy only
! aliases the actual's storage when the dummy has the TARGET attribute.
!
module m
  implicit none
  type :: t
    integer :: i
  end type
  type, extends(t) :: t2
    integer :: pad(4)
  end type
  type :: u
    integer :: k
  end type
  type :: c
    integer  :: i
    type(u)  :: sub(3)
  end type
  type, extends(c) :: c2
    integer :: pad(4)
  end type
contains
  ! A dummy (non-target): copy-in/copy-out,
  subroutine plain (x)
    type(t) :: x(:)
    if (any (x%i /= [1,2,3,4,5])) stop 1
    call expl (x)
    if (any (cshift (x%i, 1) /= [2,3,4,5,1])) stop 3
    if (any (pack (x%i, [.true.,.false.,.true.,.false.,.true.]) &
             /= [1,3,5])) stop 4
    if (any (reshape (x%i, [1,5]) /= reshape ([1,2,3,4,5], [1,5]))) stop 5
    call to_class (x)
  end subroutine

  subroutine expl (y)
    type(t) :: y(5)
    if (any (y%i /= [1,2,3,4,5])) stop 2
  end subroutine

  subroutine to_class (z)
    class(t) :: z(:)
    if (any (z%i /= [1,2,3,4,5])) stop 6
  end subroutine

  ! A component sub-array of a span-carrying dummy has its own element
  ! size and must not inherit the parent's span.
  subroutine comp (x)
    type(c), target :: x(:)
    call inner (x(2)%sub)
  end subroutine

  subroutine inner (s)
    type(u) :: s(:)
    if (any (s%k /= [21,22,23])) stop 7
  end subroutine
end module

program class_to_type_6
  use m
  implicit none
  class(t), target, allocatable :: a(:)
  class(c), target, allocatable :: b(:)
  type(t), pointer :: p
  integer :: n

  allocate (t2 :: a(5))
  do n = 1, 5
    a(n)%i = n
  end do
  call plain (a)

  allocate (c2 :: b(3))
  do n = 1, 3
    b(n)%i = 10 * n
    b(n)%sub(:)%k = [10*n+1, 10*n+2, 10*n+3]
  end do
  call comp (b)

  ! A TARGET assumed-shape dummy without CONTIGUOUS does alias.
  call aliased (a)
  if (p%i /= 3) stop 8
  a(3)%i = 999
  if (p%i /= 999) stop 9

contains
  subroutine aliased (x)
    type(t), target :: x(:)
    p => x(3)
  end subroutine
end program
