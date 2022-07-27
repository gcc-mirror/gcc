! { dg-do compile }
! { dg-options "-fopenmp" }

module m
implicit none

integer :: i

interface
  integer function bar (x,  y, z)
    integer :: x, y
    integer, value :: z
    !$omp declare simd linear (x : ref, step (1)) linear (y : step (2), uval)
  end

  integer function baz (x, y, z)
    integer :: x
    integer, value :: y, z
    !$omp declare simd linear (x : step (1), uval)
  end

  integer function qux (x, ref)
    integer :: x
    integer, value :: ref
    !$omp declare simd linear (ref (x) : ref) uniform (ref)
  end

  integer function corge (x, ref)
    integer :: x
    integer, value :: ref
    !$omp declare simd linear (x : ref, step (ref)) uniform (ref)
  end

  integer function grault (x)
    integer :: x
    !$omp declare simd linear (x : ref)
  end

  integer function waldo (x)
    integer :: x
    !$omp declare simd linear (x : uval)
  end
end interface

contains

integer function step (x)
  integer, value :: x
  step = x
end

subroutine foo (x, y)
  integer :: x, y
  !$omp simd linear (x: step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp simd linear (x: val, step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do linear (x: step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do linear (x: step (y + 1), val)
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do simd linear (x: step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do simd linear (x: val, step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do simd linear (x: step (1) + 0)
  do i = 0, 9
    x = x + step (1) + 0
  end do

  block
    integer, parameter :: ref = 1, uval = 2
    !$omp parallel do simd linear (x: ref + 0)
    do i = 0, 9
      x = x + ref + 0
    end do

    !$omp parallel do simd linear (x: uval * 1)
    do i = 0, 9
      x = x + uval
    end do
  end block
end
end
