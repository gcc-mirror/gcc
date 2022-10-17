! { dg-do compile }
! PR fortran/103473 - ICE in simplify_minmaxloc_nodim
! Test case by Gerhard Steinmetz.

subroutine s
  implicit none
  integer, parameter :: a(+'1') = [1] ! { dg-error "unary numeric operator" }
  print *, minloc (a)
end

! { dg-prune-output "Parameter array" }
