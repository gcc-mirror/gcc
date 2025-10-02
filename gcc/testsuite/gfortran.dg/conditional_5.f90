! { dg-do compile }
! { dg-options "-std=f2018" }
program conditional_std
  implicit none
  integer :: i = 42
  i = (i > 0 ? 1 : -1) ! { dg-error "Fortran 2023: Conditional expression at" }
end program conditional_std
