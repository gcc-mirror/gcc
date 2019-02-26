! { dg-additional-options "-Wuninitialized" }

subroutine acc_parallel
  implicit none
  integer :: i, j, k

  !$acc parallel loop gang num_gangs(i) ! { dg-warning "is used uninitialized in this function" }
  do i = 0, 1
  end do
  !$acc end parallel loop

  !$acc parallel loop worker num_workers(j) ! { dg-warning "is used uninitialized in this function" }
  do j = 0, 1
  end do
  !$acc end parallel loop

  !$acc parallel loop vector vector_length(k) ! { dg-warning "is used uninitialized in this function" }
  do k = 0, 1
  end do
  !$acc end parallel loop
end subroutine acc_parallel

subroutine acc_kernels
  implicit none
  integer :: i, j, k

  !$acc kernels num_gangs(i) ! { dg-warning "is used uninitialized in this function" }
  !$acc end kernels

  !$acc kernels num_workers(j) ! { dg-warning "is used uninitialized in this function" }
  !$acc end kernels

  !$acc kernels vector_length(k) ! { dg-warning "is used uninitialized in this function" }
  !$acc end kernels
end subroutine acc_kernels
