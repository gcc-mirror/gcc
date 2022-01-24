! { dg-do compile }
! PR fortran/103778

program p
  use iso_c_binding, only : c_sizeof
  integer, parameter :: a = c_sizeof(z'1') ! { dg-error "cannot appear" }
end
