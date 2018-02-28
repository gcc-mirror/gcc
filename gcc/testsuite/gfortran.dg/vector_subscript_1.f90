! PR 19239.  Check for various kinds of vector subscript.  In this test,
! all vector subscripts are indexing single-dimensional arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n = 10
  integer :: i, j, calls
  integer, dimension (n) :: a, b, idx, id

  idx = (/ 3, 1, 5, 2, 4, 10, 8, 7, 6, 9 /)
  id = (/ (i, i = 1, n) /)
  b = (/ (i * 100, i = 1, n) /)

  !------------------------------------------------------------------
  ! Tests for a simple variable subscript
  !------------------------------------------------------------------

  a (idx) = b
  call test (idx, id)

  a = b (idx)
  call test (id, idx)

  a (idx) = b (idx)
  call test (idx, idx)

  !------------------------------------------------------------------
  ! Tests for constant ranges with non-default stride
  !------------------------------------------------------------------

  a (idx (1:7:3)) = b (10:6:-2)
  call test (idx (1:7:3), id (10:6:-2))

  a (10:6:-2) = b (idx (1:7:3))
  call test (id (10:6:-2), idx (1:7:3))

  a (idx (1:7:3)) = b (idx (1:7:3))
  call test (idx (1:7:3), idx (1:7:3))

  a (idx (1:7:3)) = b (idx (10:6:-2))
  call test (idx (1:7:3), idx (10:6:-2))

  a (idx (10:6:-2)) = b (idx (10:6:-2))
  call test (idx (10:6:-2), idx (10:6:-2))

  a (idx (10:6:-2)) = b (idx (1:7:3))
  call test (idx (10:6:-2), idx (1:7:3))

  !------------------------------------------------------------------
  ! Tests for subscripts of the form CONSTRANGE + CONST
  !------------------------------------------------------------------

  a (idx (1:5) + 1) = b (1:5)
  call test (idx (1:5) + 1, id (1:5))

  a (1:5) = b (idx (1:5) + 1)
  call test (id (1:5), idx (1:5) + 1)

  a (idx (6:10) - 1) = b (idx (1:5) + 1)
  call test (idx (6:10) - 1, idx (1:5) + 1)

  !------------------------------------------------------------------
  ! Tests for variable subranges
  !------------------------------------------------------------------

  do j = 5, 10
    a (idx (2:j:2)) = b (3:2+j/2)
    call test (idx (2:j:2), id (3:2+j/2))

    a (3:2+j/2) = b (idx (2:j:2))
    call test (id (3:2+j/2), idx (2:j:2))

    a (idx (2:j:2)) = b (idx (2:j:2))
    call test (idx (2:j:2), idx (2:j:2))
  end do

  !------------------------------------------------------------------
  ! Tests for function vectors
  !------------------------------------------------------------------

  calls = 0

  a (foo (5, calls)) = b (2:10:2)
  call test (foo (5, calls), id (2:10:2))

  a (2:10:2) = b (foo (5, calls))
  call test (id (2:10:2), foo (5, calls))

  a (foo (5, calls)) = b (foo (5, calls))
  call test (foo (5, calls), foo (5, calls))

  if (calls .ne. 8) STOP 1

  !------------------------------------------------------------------
  ! Tests for constant vector constructors
  !------------------------------------------------------------------

  a ((/ 1, 5, 3, 9 /)) = b (1:4)
  call test ((/ 1, 5, 3, 9 /), id (1:4))

  a (1:4) = b ((/ 1, 5, 3, 9 /))
  call test (id (1:4), (/ 1, 5, 3, 9 /))

  a ((/ 1, 5, 3, 9 /)) = b ((/ 2, 5, 3, 7 /))
  call test ((/ 1, 5, 3, 9 /), (/ 2, 5, 3, 7 /))

  !------------------------------------------------------------------
  ! Tests for variable vector constructors
  !------------------------------------------------------------------

  do j = 1, 5
    a ((/ 1, (i + 3, i = 2, j) /)) = b (1:j)
    call test ((/ 1, (i + 3, i = 2, j) /), id (1:j))

    a (1:j) = b ((/ 1, (i + 3, i = 2, j) /))
    call test (id (1:j), (/ 1, (i + 3, i = 2, j) /))

    a ((/ 1, (i + 3, i = 2, j) /)) = b ((/ 8, (i + 2, i = 2, j) /))
    call test ((/ 1, (i + 3, i = 2, j) /), (/ 8, (i + 2, i = 2, j) /))
  end do

  !------------------------------------------------------------------
  ! Tests in which the vector dimension is partnered by a temporary
  !------------------------------------------------------------------

  calls = 0
  a (idx (1:6)) = foo (6, calls)
  if (calls .ne. 1) STOP 2
  do i = 1, 6
    if (a (idx (i)) .ne. i + 3) STOP 3
  end do
  a = 0

  calls = 0
  a (idx (1:6)) = foo (6, calls) * 100
  if (calls .ne. 1) STOP 4
  do i = 1, 6
    if (a (idx (i)) .ne. (i + 3) * 100) STOP 5
  end do
  a = 0

  a (idx) = id + 100
  do i = 1, n
    if (a (idx (i)) .ne. i + 100) STOP 6
  end do
  a = 0

  a (idx (1:10:3)) = (/ 20, 10, 9, 11 /)
  if (a (idx (1)) .ne. 20) STOP 7
  if (a (idx (4)) .ne. 10) STOP 8
  if (a (idx (7)) .ne. 9) STOP 9
  if (a (idx (10)) .ne. 11) STOP 10
  a = 0

contains
  subroutine test (lhs, rhs)
    integer, dimension (:) :: lhs, rhs
    integer :: i

    if (size (lhs, 1) .ne. size (rhs, 1)) STOP 11
    do i = 1, size (lhs, 1)
      if (a (lhs (i)) .ne. b (rhs (i))) STOP 12
    end do
    a = 0
  end subroutine test

  function foo (n, calls)
    integer :: i, n, calls
    integer, dimension (n) :: foo

    calls = calls + 1
    foo = (/ (i + 3, i = 1, n) /)
  end function foo
end program main
