! { dg-do compile }

subroutine foo (a, b, p, q)
  logical, value :: a
  logical :: b
  integer :: p(:)
  integer, pointer :: q(:)
  integer :: i
  !$omp parallel if (a)
  !$omp end parallel
  !$omp parallel if (parallel:a)
  !$omp end parallel
  !$omp parallel do simd if (a)
  do i = 1, 16
  end do
  !$omp end parallel do simd
  !$omp parallel do simd if (parallel : a)
  do i = 1, 16
  end do
  !$omp end parallel do simd
  !$omp parallel do simd if (simd : a)
  do i = 1, 16
  end do
  !$omp end parallel do simd
  !$omp parallel do simd if (simd : a) if (parallel:b)
  do i = 1, 16
  end do
  !$omp end parallel do simd
  !$omp task if (a)
  !$omp end task
  !$omp task if (task: a)
  !$omp end task
  !$omp taskloop if (a)
  do i = 1, 16
  end do
  !$omp end taskloop
  !$omp taskloop if (taskloop : a)
  do i = 1, 16
  end do
  !$omp end taskloop
  !$omp taskloop simd if (a)
  do i = 1, 16
  end do
  !$omp end taskloop simd
  !$omp taskloop simd if (taskloop : a)
  do i = 1, 16
  end do
  !$omp end taskloop simd
  !$omp taskloop simd if (simd : a)
  do i = 1, 16
  end do
  !$omp end taskloop simd
  !$omp taskloop simd if (taskloop:b) if (simd : a)
  do i = 1, 16
  end do
  !$omp end taskloop simd
  !$omp target if (a)
  !$omp end target
  !$omp target if (target: a)
  !$omp end target
  !$omp target simd if (a)
  do i = 1, 16
  end do
  !$omp end target simd
  !$omp target simd if (simd : a) if (target: b)
  do i = 1, 16
  end do
  !$omp end target simd
  !$omp target teams distribute parallel do simd if (a)
  do i = 1, 16
  end do
  !$omp end target teams distribute parallel do simd
  !$omp target teams distribute parallel do simd if (parallel : a) if (target: b)
  do i = 1, 16
  end do
  !$omp end target teams distribute parallel do simd
  !$omp target teams distribute parallel do simd if (simd : a) if (target: b)
  do i = 1, 16
  end do
  !$omp end target teams distribute parallel do simd

  !$omp target data if (a) map (p(1:2))
  !$omp end target data
  !$omp target data if (target data: a) map (p(1:2))
  !$omp end target data
  !$omp target enter data if (a) map (to: p(1:2))
  !$omp target enter data if (target enter data: a) map (to: p(1:2))
  !$omp target exit data if (a) map (from: p(1:2))
  !$omp target exit data if (target exit data: a) map (from: p(1:2))
  !$omp target update if (a) to (q(1:3))
  !$omp target update if (target update:a) to (q(1:3))
  !$omp parallel
    !$omp cancel parallel if (a)
  !$omp end parallel
  !$omp parallel
    !$omp cancel parallel if (cancel:a)
  !$omp end parallel
  !$omp do
  do i = 1, 16
      !$omp cancel do if (a)
  end do
  !$omp do
  do i = 1, 16
      !$omp cancel do if (cancel: a)
  end do
  !$omp sections
    !$omp section
	!$omp cancel sections if (a)
  !$omp end sections
  !$omp sections
    !$omp section
	!$omp cancel sections if (cancel: a)
  !$omp end sections
  !$omp taskgroup
    !$omp task
      !$omp cancel taskgroup if (a)
    !$omp end task
    !$omp task
      !$omp cancel taskgroup if (cancel: a)
    !$omp end task
  !$omp end taskgroup
end
