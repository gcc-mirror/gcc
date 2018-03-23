! { dg-do run }
! Tests the patch that implements F2003 automatic allocation and
! reallocation of allocatable arrays on assignment.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  integer(4), allocatable :: a(:), b(:), c(:,:)
  integer(4) :: j
  integer(4) :: src(2:5) = [11,12,13,14]
  integer(4) :: mat(2:3,5:6)
  character(4), allocatable :: chr1(:)
  character(4) :: chr2(2) = ["abcd", "wxyz"]

  allocate(a(1))
  mat = reshape (src, [2,2])

  a = [4,3,2,1]
  if (size(a, 1) .ne. 4) STOP 1
  if (any (a .ne. [4,3,2,1])) STOP 2

  a = [((42 - i), i = 1, 10)]
  if (size(a, 1) .ne. 10) STOP 3
  if (any (a .ne. [((42 - i), i = 1, 10)])) STOP 4

  b = a
  if (size(b, 1) .ne. 10) STOP 5
  if (any (b .ne. a)) STOP 6

  a = [4,3,2,1]
  if (size(a, 1) .ne. 4) STOP 7
  if (any (a .ne. [4,3,2,1])) STOP 8

  a = b
  if (size(a, 1) .ne. 10) STOP 9
  if (any (a .ne. [((42 - i), i = 1, 10)])) STOP 10

  j = 20
  a = [(i, i = 1, j)]
  if (size(a, 1) .ne. j) STOP 11
  if (any (a .ne. [(i, i = 1, j)])) STOP 12

  a = foo (15)
  if (size(a, 1) .ne. 15) STOP 13
  if (any (a .ne. [((i + 15), i = 1, 15)])) STOP 14

  a = src
  if (lbound(a, 1) .ne. lbound(src, 1)) STOP 15
  if (ubound(a, 1) .ne. ubound(src, 1)) STOP 16
  if (any (a .ne. [11,12,13,14])) STOP 17

  k = 7
  a = b(k:8)
  if (lbound(a, 1) .ne. lbound (b(k:8), 1)) STOP 18
  if (ubound(a, 1) .ne. ubound (b(k:8), 1)) STOP 19
  if (any (a .ne. [35,34])) STOP 20

  c = mat
  if (any (lbound (c) .ne. lbound (mat))) STOP 21
  if (any (ubound (c) .ne. ubound (mat))) STOP 22
  if (any (c .ne. mat)) STOP 23

  deallocate (c)
  c = mat(2:,:)
  if (any (lbound (c) .ne. lbound (mat(2:,:)))) STOP 24

  chr1 = chr2(2:1:-1)
  if (lbound(chr1, 1) .ne. 1) STOP 25
  if (any (chr1 .ne. chr2(2:1:-1))) STOP 26

  b = c(1, :) + c(2, :)
  if (lbound(b, 1) .ne. lbound (c(1, :) + c(2, :), 1)) STOP 27
  if (any (b .ne. c(1, :) + c(2, :))) STOP 28
contains
  function foo (n) result(res)
    integer(4), allocatable, dimension(:) :: res
    integer(4) :: n
    allocate (res(n))
    res = [((i + 15), i = 1, n)]
  end function foo
end
