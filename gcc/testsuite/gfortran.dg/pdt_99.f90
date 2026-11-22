! { dg-do run }
!
! PR 128936

module pdt_99_m
  type t(n, m)
    integer, len :: n, m
    character(len=n) :: ch(2)
    character(len=n) :: s
    character(len=n+m) :: u(3)
  end type
contains
  subroutine check_assumed (a, b)
    type(t(*,*)) :: a, b(:)
    character(len=len(a%ch)) :: buf
    if (len (a%ch) /= 5) stop 20
    if (len (buf) /= 5) stop 21
    if (len (b%s) /= 5) stop 22
    if (len (b(1)%ch) /= 5) stop 23
  end subroutine
end module

program p
  use pdt_99_m
  implicit none
  type(t(4,1)) :: x, y(-1:1)
  type(t(:,:)), allocatable :: z(:)
  integer :: i, j, k

  x%ch = 'abcdef'
  i = len (x%ch)
  if (i /= 4) stop 1
  if (len (x%ch) /= 4) stop 2
  if (len (x%u) /= 5) stop 3
  if (kind (len (x%ch, kind=8)) /= 8) stop 4
  if (len (x%ch, kind=8) /= 4_8) stop 5
  if (any ([len (x%ch), len (x%u)] /= [4, 5])) stop 6
  i = 2; j = 3
  if (len (x%ch(1)(i:j)) /= 2) stop 7

  if (len (y(0)%ch) /= 4) stop 8
  if (len (y%s) /= 4) stop 9
  if (len (y(0:1)%s) /= 4) stop 10
  if (y(0:1)%n /= 4) stop 11

  k = 5
  allocate (t(k,2) :: z(3))
  if (len (z(2)%ch) /= 5) stop 12
  if (len (z(2:3)%s) /= 5) stop 13
  call check_assumed (z(1), z)
end program
