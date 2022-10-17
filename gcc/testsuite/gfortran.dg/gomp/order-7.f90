subroutine f1 (a)
  integer :: a(*)
  integer i
  !$omp do order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp simd order ( concurrent )
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do simd order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end

subroutine f2 (a)
  integer :: a(*)
  integer i
  !$omp parallel do order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp parallel do simd order (concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute parallel do order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute parallel do simd order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams
    !$omp distribute parallel do order(concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
    !$omp distribute parallel do simd order(concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
    !$omp distribute order(concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
  !$omp end teams
  !$omp taskloop simd order (concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end
