! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Array reference out of bounds" }
! PR fortran/31627
subroutine foo(a)
  integer a(*), i
  i = 0
  a(i) = 42 ! {
end subroutine foo

program test
  integer x(42)
  call foo(x)
end program test
! { dg-output "Array reference out of bounds .* lower bound of dimension 1 exceeded" }
