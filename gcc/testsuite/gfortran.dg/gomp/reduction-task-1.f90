module m
  implicit none
  integer v
  interface
    subroutine foo(x)
      integer, value :: x
    end
  end interface
contains

subroutine bar
  integer i
  !$omp do reduction (task, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp sections reduction (task, +: v)
    call foo (-2)
    !$omp section
    call foo (-3)
  !$omp end sections
  !$omp parallel reduction (task, +: v)
  call foo (-1)
  !$omp end parallel
  !$omp parallel do reduction (task, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp end parallel do
  !$omp parallel sections reduction (task, +: v)
    call foo (-2)
    !$omp section
    call foo (-3)
  !$omp end parallel sections
  !$omp teams distribute parallel do reduction (task, +: v)  ! { dg-bogus "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" "PR101948" { xfail *-*-* } }
  do i = 0, 63
    call foo (i)
  end do
  !$omp end teams distribute parallel do
  !$omp do reduction (default, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp sections reduction (default, +: v)
    call foo (-2)
    !$omp section
    call foo (-3)
  !$omp end sections
  !$omp parallel reduction (default, +: v)
  call foo (-1)
  !$omp end parallel
  !$omp parallel do reduction (default, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp end parallel do
  !$omp parallel sections reduction (default, +: v)
    call foo (-2)
    !$omp section
    call foo (-3)
  !$omp end parallel sections
  !$omp teams distribute parallel do reduction (default, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp end teams distribute parallel do
  !$omp do reduction (default, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp end do nowait
  !$omp sections reduction (default, +: v)
    call foo (-2)
    !$omp section
    call foo (-3)
  !$omp end sections nowait
  !$omp simd reduction (default, +: v)
  do i = 0, 63
    v = v + 1
  end do
  !$omp do simd reduction (default, +: v)
  do i = 0, 63
    v = v + 1
  end do
  !$omp parallel do simd reduction (default, +: v)
  do i = 0, 63
    v = v + 1
  end do
  !$omp end parallel do simd
  !$omp teams distribute parallel do simd reduction (default, +: v)
  do i = 0, 63
    v = v + 1
  end do
  !$omp end teams distribute parallel do simd
  !$omp taskloop reduction (default, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp taskloop simd reduction (default, +: v)
  do i = 0, 63
    v = v + 1
  end do
  !$omp teams reduction (default, +: v)
  call foo (i)
  !$omp end teams
  !$omp teams distribute reduction (default, +: v)
  do i = 0, 63
    call foo (i)
  end do
  !$omp end teams distribute
end
end
