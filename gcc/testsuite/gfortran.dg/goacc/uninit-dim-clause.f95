! { dg-additional-options "-Wuninitialized" }

subroutine acc_parallel
  implicit none
  integer :: i, j, k

  !$acc parallel num_gangs(i) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel

  !$acc parallel num_workers(j) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel

  !$acc parallel vector_length(k) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel
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
