! Like vector_subscript_1.f90, but check subscripts in multi-dimensional
! arrays.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n = 5
  integer :: i1, i2, i3
  integer, dimension (n, n, n) :: a, b
  integer, dimension (n) :: idx, id

  idx = (/ 3, 1, 5, 2, 4 /)
  id = (/ (i1, i1 = 1, n) /)
  forall (i1 = 1:n, i2 = 1:n, i3 = 1:n)
    b (i1, i2, i3) = i1 + i2 * 10 + i3 * 100
  end forall

  i1 = 5
  a (foo (i1), 1, :) = b (2, :, foo (i1))
  do i1 = 1, 5
    do i2 = 1, 5
      if (a (idx (i1), 1, i2) .ne. b (2, i1, idx (i2))) STOP 1
    end do
  end do
  a = 0

  a (1, idx (1:4), 2:4) = b (2:5, idx (3:5), 2)
  do i1 = 1, 4
    do i2 = 1, 3
      if (a (1, idx (i1), 1 + i2) .ne. b (1 + i1, idx (i2 + 2), 2)) STOP 2
    end do
  end do
  a = 0
contains
  function foo (n)
    integer :: n
    integer, dimension (n) :: foo
    foo = idx (1:n)
  end function foo
end program main
