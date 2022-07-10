! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-original" }

module m
  implicit none (type, external)

  integer i

  interface
    integer function bar (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, step (1)) linear (y : step (2))
    end

    integer function baz (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : step (1), val)
    end

    integer function qux (x, val)
      integer, value :: x, val
      !$omp declare simd linear (val (x) : val) uniform (val)
    end

    integer function corge (x, val)
      integer, value :: x, val
      !$omp declare simd linear (x : val, step (val)) uniform (val)
    end

    integer function grault (x)
      integer, value :: x
      !$omp declare simd linear (x : val)
    end

    integer function step (x)
      integer, value :: x
    end
  end interface

contains

subroutine foo (x,y)
  integer :: x, y
  integer :: val

  val = 1

  !$omp simd linear (i: step (3))
  do i = 0, 32, 3
  end do

  !$omp simd linear (i: val, step (3))
  do i = 0, 32, 3
  end do

  !$omp simd linear (x: step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp simd linear (x: step (y + 1), val)
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do linear (x: step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do linear (x: val, step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do simd linear (i: step (3))
  do i = 0, 32, 3
  end do

  !$omp parallel do simd linear (i: step (3), val)
  do i = 0, 32, 3
  end do

  !$omp parallel do simd linear (x: step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do simd linear (x: val, step (y + 1))
  do i = 0, 9
    x = x + y + 1
  end do

  !$omp parallel do simd linear (i: val + 0)
  do i = 0, 9
  end do

  !$omp parallel do simd linear (i: step (1) * 1)
  do i = 0, 9
  end do
end
end module

! { dg-final { scan-tree-dump-times "#pragma omp parallel" 8 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 6 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for linear\\(x:D\\.\[0-9\]+\\) nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for linear\\(x:val,step\\(D\\.\[0-9\]+\\)\\) nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(count\\.\[0-9\]:1\\) linear\\(i:3\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(count\\.\[0-9\]:1\\) linear\\(i:val,step\\(3\\)\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) linear\\(x:D\\.\[0-9\]+\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) linear\\(x:val,step\\(D\\.\[0-9\]+\\)\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:D\\.\[0-9\]+\\)" 2 "original" } }
