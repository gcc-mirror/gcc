! { dg-do compile }
! PR fortran/34536 -- unary operators following arithmetic ones

  real :: x
  x = 2.0 ** -3 * 5  ! { dg-warning "Unary operator following arithmetic operator" }
end