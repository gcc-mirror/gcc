! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

program test
  implicit none
  logical :: b, b2, bs, b3, b4
  ! { dg-note {'b' was declared here} {} { target *-*-* } .-1 }
  ! { dg-note {'b2' was declared here} {} { target *-*-* } .-2 }
  ! { dg-note {'bs' was declared here} {} { target *-*-* } .-3 }
  ! { dg-note {'b3' was declared here} {} { target *-*-* } .-4 }
  ! { dg-note {'b4' was declared here} {} { target *-*-* } .-5 }
  integer :: data, data2

  !$acc parallel if(b) ! { dg-warning "is used uninitialized" }
  !$acc end parallel

  !$acc kernels if(b2) ! { dg-warning "is used uninitialized" }
  !$acc end kernels

  !$acc serial if(bs) ! { dg-warning "is used uninitialized" }
  !$acc end serial

  !$acc data if(b3) ! { dg-warning "is used uninitialized" }
  !$acc end data

  !$acc update if(b4) self(data2) ! { dg-warning "is used uninitialized" }

end program test
