! { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose" }

program main
  implicit none
  integer, parameter         :: N = 1024
  integer, dimension (1:N)   :: a
  integer                    :: i, sum

  !$acc kernels copyin(a(1:N)) copy(sum)

  ! converted to "oacc_kernels"
  !$acc loop
  do i = 1, N
    sum = sum + a(i)
  end do

  ! converted to "oacc_parallel_kernels_gang_single"
  sum = sum + 1
  a(1) = a(1) + 1

  ! converted to "oacc_parallel_kernels_parallelized"
  !$acc loop independent
  do i = 1, N
    sum = sum + a(i)
  end do

  ! converted to "oacc_kernels"
  if (sum .gt. 10) then
    !$acc loop
    do i = 1, N
      sum = sum + a(i)
    end do
  end if

  ! converted to "oacc_kernels"
  !$acc loop auto
  do i = 1, N
    sum = sum + a(i)
  end do

  !$acc end kernels
end program main

! Check that the kernels region is split into a data region and enclosed
! parallel regions.
! { dg-final { scan-tree-dump-times "oacc_data_kernels" 1 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels_graphite " 5 "omp_oacc_kernels_decompose" } }

! Each of the parallel regions is async, and there is a final call to
! __builtin_GOACC_wait.
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels_graphite async\\(-1\\)" 5 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_wait" 1 "omp_oacc_kernels_decompose" } }
