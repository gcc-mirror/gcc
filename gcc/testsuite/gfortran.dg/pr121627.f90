! { dg-do compile }
program real_kinds         ! { dg-error "already declared at" }
  use iso_fortran_env      ! { dg-error "already declared at" }
  i = real64
end program real_kinds
