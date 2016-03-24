! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

program test
  implicit none
  logical :: b, b2, b3, b4
  integer :: data, data2

  !$acc parallel if(b) ! { dg-warning "is used uninitialized in this function" }
  !$acc end parallel

  !$acc kernels if(b2) ! { dg-warning "is used uninitialized in this function" }
  !$acc end kernels

  !$acc data if(b3) ! { dg-warning "is used uninitialized in this function" }
  !$acc end data

  !$acc update if(b4) self(data2) ! { dg-warning "is used uninitialized in this function" }

end program test
