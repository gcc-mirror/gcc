! { dg-do compile }
! { dg-skip-if "NaN not supported" { spu-*-* } }
! PR fortran/37930
program test
  implicit none
  integer i
  i = transfer(-1,1.0) ! { dg-error "Conversion" }
end program test
