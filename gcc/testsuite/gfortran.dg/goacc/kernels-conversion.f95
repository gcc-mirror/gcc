! { dg-additional-options "-fopenacc-kernels=split" }
! { dg-additional-options "-fdump-tree-convert_oacc_kernels" }

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
! { dg-final { scan-tree-dump-times "oacc_data_kernels" 1 "convert_oacc_kernels" } }

! As noted in the comments above, we get one gang-single serial region; one
! parallelized loop region; and three "old-style" kernel regions.
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels_gang_single" 1 "convert_oacc_kernels" } }
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels_parallelized" 1 "convert_oacc_kernels" } }
! { dg-final { scan-tree-dump-times "oacc_kernels" 3 "convert_oacc_kernels" } }

! Each of the parallel regions is async, and there is a final call to
! __builtin_GOACC_wait.
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels.* async\(-1\)" 5 "convert_oacc_kernels" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_wait" 1 "convert_oacc_kernels" } }
