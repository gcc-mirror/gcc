! { dg-additional-options "-fopenacc-kernels=split" }
! { dg-additional-options "-fdump-tree-convert_oacc_kernels" }

program main
  implicit none
  integer, parameter         :: N = 1024
  integer, dimension (1:N)   :: a
  integer                    :: i, sum

  !$acc kernels copyin(a(1:N)) copy(sum)

  !$acc loop
  do i = 1, N
    sum = sum + a(i)
  end do

  sum = sum + 1
  a(1) = a(1) + 1

  !$acc loop
  do i = 1, N
    sum = sum + a(i)
  end do

  if (sum .gt. 10) then
    !$acc loop
    do i = 1, N
      sum = sum + a(i)
    end do
  end if

  !$acc loop
  ! { dg-bogus "region contains gang partitoned code but is not gang partitioned" "gang partitioned" { xfail *-*-* } .-1 }
  do i = 1, N
    sum = sum + a(i)
  end do

  !$acc end kernels
end program main

! Check that the kernels region is split into a data region and enclosed
! parallel regions.
! { dg-final { scan-tree-dump-times "oacc_data_kernels" 1 "convert_oacc_kernels" } }

! The three unconditional loop regions are parallelized, the sequential part
! in between and the conditional loop are made gang-single.
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels_parallelized" 3 "convert_oacc_kernels" } }
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels_gang_single" 2 "convert_oacc_kernels" } }

! Each of the parallel regions is async, and there is a final call to
! __builtin_GOACC_wait.
! { dg-final { scan-tree-dump-times "oacc_parallel_kernels.* async\(-1\)" 5 "convert_oacc_kernels" } }
! { dg-final { scan-tree-dump-times "__builtin_GOACC_wait" 1 "convert_oacc_kernels" } }

! Check that the original kernels region is removed.
! { dg-final { scan-tree-dump-not "oacc_kernels" "convert_oacc_kernels" } }
