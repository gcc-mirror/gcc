! { dg-do compile }
! { dg-options "-fopenmp" }

module m
  implicit none (type, external)

  interface
    integer function step (x, y, z)
      integer :: x, y, z
    end function step
  end interface

contains

integer function foo (x)
  integer, value :: x
  integer :: i
  !$omp parallel do linear (x : step (step (1, 2, 3)))
  do i = 0, 63
    x = x + 6
  end do
  foo = x
end

integer function bar (x)
  integer, value :: x
  integer :: i
  !$omp parallel do linear (x : step (1, 2, 3))	! { dg-error "40: Invalid character in name" }
  do i = 0, 63
    x = x + 6
  end do
  bar = x
end

integer function bar2 (x)
  integer, value :: x
  integer :: i
  !$omp parallel do linear (x : step (1, 2, 3) * 1)
  do i = 0, 63
    x = x + 6
  end do
  bar2 = x
end
end module
