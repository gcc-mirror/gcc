! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

program test
  implicit none
  integer :: i, j, k

  !$acc parallel num_gangs(i) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel

  !$acc parallel num_workers(j) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel

  !$acc parallel vector_length(k) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel

end program test
