! { dg-additional-options "-Wuninitialized" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

subroutine acc_parallel
  implicit none
  integer :: i, j, k

  !$acc parallel num_gangs(i) ! { dg-warning "is used uninitialized" }
  ! { dg-warning "region is gang partitioned but does not contain gang partitioned code" "" { target *-*-* } .-1 }
  !$acc end parallel

  !$acc parallel num_workers(j) ! { dg-warning "is used uninitialized" }
  ! { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-1 }
  !$acc end parallel

  !$acc parallel vector_length(k) ! { dg-warning "is used uninitialized" }
  ! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-1 }
  !$acc end parallel
end subroutine acc_parallel

subroutine acc_kernels
  implicit none
  integer :: i, j, k

  !$acc kernels num_gangs(i) ! { dg-warning "is used uninitialized" }
  !$acc end kernels

  !$acc kernels num_workers(j) ! { dg-warning "is used uninitialized" }
  !$acc end kernels

  !$acc kernels vector_length(k) ! { dg-warning "is used uninitialized" }
  !$acc end kernels
end subroutine acc_kernels
