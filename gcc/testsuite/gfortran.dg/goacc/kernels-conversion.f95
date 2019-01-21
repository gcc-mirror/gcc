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

  !$acc loop
  do i = 1, N
    sum = sum + a(i)
  end do

  !$acc end kernels
end program main

! Check that the kernels region is split into a data region and an enclosed
! parallel region.
! { dg-final { scan-tree-dump-times "oacc_data_kernels" 1 "convert_oacc_kernels" } }
! { dg-final { scan-tree-dump-times "oacc_parallel" 1 "convert_oacc_kernels" } }

! Check that the original kernels region is removed.
! { dg-final { scan-tree-dump-not "oacc_kernels" "convert_oacc_kernels" } }
