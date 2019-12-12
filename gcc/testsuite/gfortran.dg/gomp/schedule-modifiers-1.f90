! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo
  integer :: i
  !$omp do simd schedule (simd, simd: static, 5)
  do i = 0, 64
  end do
  !$omp do simd schedule (monotonic, simd: static)
  do i = 0, 64
  end do
  !$omp do simd schedule (simd , monotonic : static, 6)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic, monotonic : static, 7)
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic, nonmonotonic : dynamic)
  do i = 0, 64
  end do
  !$omp do simd schedule (nonmonotonic , simd : dynamic, 3)
  do i = 0, 64
  end do
  !$omp do simd schedule (nonmonotonic,simd:guided,4)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic: static, 2)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : static)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : dynamic)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : dynamic, 3)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : guided)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : guided, 7)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : runtime)
  do i = 0, 64
  end do
  !$omp do schedule (monotonic : auto)
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : dynamic)
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : dynamic, 3)
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : guided)
  do i = 0, 64
  end do
  !$omp do schedule (nonmonotonic : guided, 7)
  do i = 0, 64
  end do
end subroutine foo
