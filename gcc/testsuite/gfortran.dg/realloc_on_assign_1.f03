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
  if (size(a, 1) .ne. 4) call abort
  if (any (a .ne. [4,3,2,1])) call abort

  a = [((42 - i), i = 1, 10)]
  if (size(a, 1) .ne. 10) call abort
  if (any (a .ne. [((42 - i), i = 1, 10)])) call abort

  b = a
  if (size(b, 1) .ne. 10) call abort
  if (any (b .ne. a)) call abort

  a = [4,3,2,1]
  if (size(a, 1) .ne. 4) call abort
  if (any (a .ne. [4,3,2,1])) call abort

  a = b
  if (size(a, 1) .ne. 10) call abort
  if (any (a .ne. [((42 - i), i = 1, 10)])) call abort

  j = 20
  a = [(i, i = 1, j)]
  if (size(a, 1) .ne. j) call abort
  if (any (a .ne. [(i, i = 1, j)])) call abort

  a = foo (15)
  if (size(a, 1) .ne. 15) call abort
  if (any (a .ne. [((i + 15), i = 1, 15)])) call abort

  a = src
  if (lbound(a, 1) .ne. lbound(src, 1)) call abort
  if (ubound(a, 1) .ne. ubound(src, 1)) call abort
  if (any (a .ne. [11,12,13,14])) call abort

  k = 7
  a = b(k:8)
  if (lbound(a, 1) .ne. lbound (b(k:8), 1)) call abort
  if (ubound(a, 1) .ne. ubound (b(k:8), 1)) call abort
  if (any (a .ne. [35,34])) call abort

  c = mat
  if (any (lbound (c) .ne. lbound (mat))) call abort
  if (any (ubound (c) .ne. ubound (mat))) call abort
  if (any (c .ne. mat)) call abort

  deallocate (c)
  c = mat(2:,:)
  if (any (lbound (c) .ne. lbound (mat(2:,:)))) call abort

  chr1 = chr2(2:1:-1)
  if (lbound(chr1, 1) .ne. 1) call abort
  if (any (chr1 .ne. chr2(2:1:-1))) call abort

  b = c(1, :) + c(2, :)
  if (lbound(b, 1) .ne. lbound (c(1, :) + c(2, :), 1)) call abort
  if (any (b .ne. c(1, :) + c(2, :))) call abort
contains
  function foo (n) result(res)
    integer(4), allocatable, dimension(:) :: res
    integer(4) :: n
    allocate (res(n))
    res = [((i + 15), i = 1, n)]
  end function foo
end
