! { dg-do compile }
! { dg-options "-funsigned" }
! Test that overflow warned about.
program main
  unsigned(1) :: u
  u = 256u_1 ! { dg-warning "Unsigned constant truncated" }
  u = -127u_1
  u = 255u_1
  u = -129u_1 ! { dg-warning "Unsigned constant truncated" }
end
