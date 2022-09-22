! { dg-do compile }
! PR fortran/99349 - ICE in match_data_constant
! Contributed by G.Steinmetz

function f()
  logical, parameter :: a((1.)/0) = .true. ! { dg-error "Parameter array" }
  integer :: b
  data b /a%kind/ ! { dg-error "Syntax error" }
end
