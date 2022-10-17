module m
  integer :: v
  interface
    subroutine foo(i)
      integer :: i
    end
  end interface
end

subroutine bar
  use m
  implicit none
  integer :: i
  !$omp do reduction (task, +: v)  ! { dg-error "'task' reduction modifier on a construct with a 'nowait' clause" }
  do i = 0, 63
    call foo (i)
  end do
  !$omp end do nowait
  !$omp sections reduction (task, +: v)	! { dg-error "'task' reduction modifier on a construct with a 'nowait' clause" }
    call foo (-2)
    !$omp section
    call foo (-3)
  !$omp end sections nowait
  !$omp scope reduction (task, +: v)	! { dg-error "'task' reduction modifier on a construct with a 'nowait' clause" }
  call foo (-4)
  !$omp end scope nowait
  !$omp simd reduction (task, +: v)	! { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" }
  do i = 0, 63
    v = v + 1
  end do
  !$omp do simd reduction (task, +: v)	! { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" }
  do i = 0, 63
    v = v + 1
  end do
  !$omp parallel do simd reduction (task, +: v)	! { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" }
  do i = 0, 63
    v = v + 1
  end do
  !$omp end parallel do simd
  !$omp teams distribute parallel do simd reduction (task, +: v)	! { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" }
  do i = 0, 63
    v = v + 1
  end do
  !$omp end teams distribute parallel do simd
end
