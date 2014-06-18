! { dg-do compile }
! { dg-options "-fopenmp -ffree-line-length-160" }

subroutine foo (n, s, t, u, v, w)
  integer :: n, i, s, t, u, v, w
  common /bar/ i
  !$omp simd safelen(s + 1)
  do i = 1, n
  end do
  !$omp do schedule (static, t * 2)
  do i = 1, n
  end do
  !$omp do simd safelen(s + 1) schedule (static, t * 2)
  do i = 1, n
  end do
  !$omp parallel do schedule (static, t * 2) num_threads (u - 1)
  do i = 1, n
  end do
  !$omp parallel do simd safelen(s + 1) schedule (static, t * 2) num_threads (u - 1)
  do i = 1, n
  end do
  !$omp distribute dist_schedule (static, v + 8)
  do i = 1, n
  end do
  !$omp distribute simd dist_schedule (static, v + 8) safelen(s + 1)
  do i = 1, n
  end do
  !$omp distribute parallel do simd dist_schedule (static, v + 8) safelen(s + 1) &
  !$omp & schedule (static, t * 2) num_threads (u - 1)
  do i = 1, n
  end do
  !$omp distribute parallel do dist_schedule (static, v + 8) num_threads (u - 1) &
  !$omp & schedule (static, t * 2)
  do i = 1, n
  end do
  !$omp target
  !$omp teams distribute dist_schedule (static, v + 8) num_teams (w + 8)
  do i = 1, n
  end do
  !$omp end target
  !$omp target
  !$omp teams distribute simd dist_schedule (static, v + 8) safelen(s + 1) &
  !$omp & num_teams (w + 8)
  do i = 1, n
  end do
  !$omp end target
  !$omp target
  !$omp teams distribute parallel do simd dist_schedule (static, v + 8) safelen(s + 1) &
  !$omp & schedule (static, t * 2) num_threads (u - 1) num_teams (w + 8)
  do i = 1, n
  end do
  !$omp end target
  !$omp target
  !$omp teams distribute parallel do dist_schedule (static, v + 8) num_threads (u - 1) &
  !$omp & schedule (static, t * 2) num_teams (w + 8)
  do i = 1, n
  end do
  !$omp end target
  !$omp target teams distribute dist_schedule (static, v + 8) num_teams (w + 8)
  do i = 1, n
  end do
  !$omp target teams distribute simd dist_schedule (static, v + 8) safelen(s + 1) &
  !$omp & num_teams (w + 8)
  do i = 1, n
  end do
  !$omp target teams distribute parallel do simd dist_schedule (static, v + 8) safelen(s + 1) &
  !$omp & schedule (static, t * 2) num_threads (u - 1) num_teams (w + 8)
  do i = 1, n
  end do
  !$omp target teams distribute parallel do dist_schedule (static, v + 8) num_threads (u - 1) &
  !$omp & schedule (static, t * 2) num_teams (w + 8)
  do i = 1, n
  end do
end subroutine
