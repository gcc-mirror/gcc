! { dg-do compile }
! PR fortran/103505 - this used to ICE in compare_bound_mpz_t
! Testcase by G.Steinmetz

program p
  integer, parameter :: a((2.))   = [4,8] ! { dg-error "INTEGER type" }
  integer, parameter :: z(1:(2.)) = [4,8] ! { dg-error "INTEGER type" }
  print *, a(1:1)
end

! { dg-prune-output "Parameter array" }
