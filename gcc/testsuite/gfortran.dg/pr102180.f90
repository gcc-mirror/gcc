! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/102180 - Improve checking of assumed size array spec

subroutine s(x,y)
  real :: x(0:*) ! legal
  real :: y[0:*] ! legal
end

subroutine t(x,y)
  real :: x(:*) ! { dg-error "A lower bound must precede colon" }
  real :: y[:*] ! { dg-error "A lower bound must precede colon" }
end

subroutine u(x,y,z)
  real :: x(2,*)
  real :: y(2,2:*)
  real :: z(2,:*) ! { dg-error "A lower bound must precede colon" }
end
