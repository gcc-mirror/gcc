! { dg-do compile }
! PR fortran/37930
program test
  implicit none
  integer i
  i = transfer(-1,1.0) ! { dg-error "Conversion" }
end program test
