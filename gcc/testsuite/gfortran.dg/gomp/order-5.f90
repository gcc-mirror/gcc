! { dg-additional-options "-fdump-tree-original" }

subroutine f1 (a)
  integer :: a(*), i
  !$omp do order(reproducible:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp simd order ( reproducible : concurrent )
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do simd order(reproducible :concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end

subroutine f2 (a)
  integer :: a(*), i
  !$omp parallel do order(reproducible: concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp parallel do simd order (reproducible:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute parallel do order(reproducible:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute parallel do simd order(reproducible:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute order(reproducible:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams
    !$omp distribute parallel do order(reproducible:concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
    !$omp distribute parallel do simd order(reproducible:concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
    !$omp distribute order(reproducible:concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
  !$omp end teams
  !$omp taskloop simd order (reproducible:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end

subroutine f3 (a)
  integer :: a(*), i
  !$omp do order(unconstrained:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp simd order ( unconstrained : concurrent )
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do simd order(unconstrained :concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end

subroutine f4 (a)
  integer :: a(*), i
  !$omp parallel do order(unconstrained: concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp parallel do simd order (unconstrained:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute parallel do order(unconstrained:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute parallel do simd order(unconstrained:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams distribute order(unconstrained:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp teams
    !$omp distribute parallel do order(unconstrained:concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
    !$omp distribute parallel do simd order(unconstrained:concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
    !$omp distribute order(unconstrained:concurrent)
    do i = 1, 128
      a(i) = a(i) + 1
    end do
  !$omp end teams
  !$omp taskloop simd order (unconstrained:concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end

! { dg-final { scan-tree-dump-times "#pragma omp distribute order\\(reproducible:concurrent\\)" 6 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp distribute order\\(unconstrained:concurrent\\)" 6 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait order\\(reproducible:concurrent\\)" 6 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait order\\(unconstrained:concurrent\\)" 6 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp for order\\(reproducible:concurrent\\)" 2 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp for order\\(unconstrained:concurrent\\)" 2 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 12 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) order\\(reproducible:concurrent\\)" 6 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) order\\(unconstrained:concurrent\\)" 6 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp taskloop" 2 "original"} }
! { dg-final { scan-tree-dump-times "#pragma omp teams" 8 "original"} }
