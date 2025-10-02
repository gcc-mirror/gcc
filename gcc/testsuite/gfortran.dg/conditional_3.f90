! { dg-do compile }
! { dg-options "-std=f2023" }
program conditional_syntax
  implicit none
  integer :: i = 42

  i = i > 0 ? 1 : -1 ! { dg-error "Unclassifiable statement at" }
  i = (i > 0 ? 1 -1) ! { dg-error "Expected ':' in conditional expression" }
end program conditional_syntax
