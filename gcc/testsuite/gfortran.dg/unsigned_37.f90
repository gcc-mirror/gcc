! { dg-do compile }
program main
  use iso_fortran_env, only : uint32 ! { dg-error "not in the selected standard" }
end program main
