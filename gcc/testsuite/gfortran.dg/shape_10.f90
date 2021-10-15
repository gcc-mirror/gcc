! { dg-do compile }
! PR fortran/102716

program p
  integer, parameter :: a(1) = shape([2], [1]) ! { dg-error "must be a scalar" }
end
