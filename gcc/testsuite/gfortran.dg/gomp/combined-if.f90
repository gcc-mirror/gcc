! { dg-do compile }
! { dg-additional-options "-fdump-tree-omplower" }

module combined_if
  implicit none

  integer, parameter :: N = 100
  integer, parameter :: LIMIT = 60
  integer :: i, j
  integer, dimension(N) :: a = (/ (i, i = 1,N) /)
contains
  subroutine test_parallel_loop_simd
    do j = 1, N
      !$omp parallel do simd if(j .lt. LIMIT)
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

  subroutine test_target_parallel
    do j = 1, N
      !$omp target parallel if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
      !$omp end target parallel
     end do
  end subroutine

  subroutine test_target_parallel_loop
    do j = 1, N
      !$omp target parallel do if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

  subroutine test_target_parallel_loop_simd
    do j = 1, N
      !$omp target parallel do simd if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

  subroutine test_target_simd
    do j = 1, N
      !$omp target simd if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

  subroutine test_target_teams
    do j = 1, N
      !$omp target teams if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
      !$omp end target teams
    end do
  end subroutine

  subroutine test_target_teams_distribute
    do j = 1, N
      !$omp target teams distribute if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

  subroutine test_target_teams_distibute_simd
    do j = 1, N
      !$omp target teams distribute simd if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

  subroutine test_target_teams_distribute_parallel_loop
    do j = 1, N
      !$omp target teams distribute parallel do if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine
    
  subroutine test_target_teams_distribute_parallel_loop_simd
    do j = 1, N
      !$omp target teams distribute parallel do simd if(j .lt. LIMIT) map(tofrom: a(1:N))
      do i = 1, N
        a(i) = a(i) + 1
      end do
    end do
  end subroutine

end module

! { dg-final { scan-tree-dump-times "(?n)#pragma omp target.* if\\(" 9 "omplower" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp simd.* if\\(" 5 "omplower" { target { ! offload_nvptx } } } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp simd.* if\\(" 9 "omplower" { target { offload_nvptx } } } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp parallel.* if\\(" 6 "omplower" } }
