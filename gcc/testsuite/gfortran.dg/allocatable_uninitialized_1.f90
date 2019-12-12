! { dg-do compile }
! { dg-options "-O -Wall" }
program main
  real,allocatable:: a(:),b(:)

   a(1)=2*b(1) ! { dg-warning "uninitialized" }

end
