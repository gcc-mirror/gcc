! { dg-do compile }
! PR fortran/103259 - ICE in resolve_common_vars
! Contributed by G.Steinmetz

module m
  integer :: p
  common /c/ p
end
program p ! { dg-error "cannot appear in a COMMON block" }
  use m   ! { dg-error "is also the name of the current program unit" }
end
