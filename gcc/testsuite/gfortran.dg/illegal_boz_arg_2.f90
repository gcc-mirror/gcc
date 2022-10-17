! { dg-do compile }
! PR fortran/103412

program p
  integer, parameter :: a = sizeof(z'1') ! { dg-error "cannot be an actual" }
end
