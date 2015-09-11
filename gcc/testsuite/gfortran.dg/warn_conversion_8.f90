! { dg-do compile }
! { dg-options "-Winteger-division" }
program main
  integer, parameter :: n = 23
  integer, parameter :: m = n*(n+1)/2  ! No warning
  integer, parameter :: i = n*(n+1)/17 ! { dg-warning "Integer division truncated to constant" }
  print *, 3/5 ! { dg-warning "Integer division truncated to constant" }
end program main
