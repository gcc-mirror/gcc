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
! { dg-output "Index '0' of dimension 1 of array 'a' below lower bound of 1" }
