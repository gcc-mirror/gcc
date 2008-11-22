! { dg-do compile }
!
! PR fortran/38160
!

subroutine foo(x,y,z,a) bind(c) ! { dg-warning "but may not be C interoperable" }
  use iso_c_binding
  implicit none
  integer(4) :: x
  integer(c_float) :: y ! { dg-error "C kind type parameter is for type REAL" }
  complex(c_float) :: z ! OK, c_float == c_float_complex
  real(c_float_complex) :: a ! OK, c_float == c_float_complex
end subroutine foo

use iso_c_binding
implicit none
integer, parameter :: it = c_int
integer, parameter :: dt = c_double
complex(c_int), target    :: z1  ! { dg-error "C kind type parameter is for type INTEGER" }
complex(it), target       :: z2  ! { dg-error "C kind type parameter is for type INTEGER" }
complex(c_double), target :: z3  ! OK
complex(dt), target       :: z4  ! OK
type(c_ptr) :: ptr

ptr = c_loc(z1)
ptr = c_loc(z2)
ptr = c_loc(z3)
ptr = c_loc(z4)
end
