! { dg-do compile }
! PR fortran/103505 - this used to ICE in compare_bound_mpz_t
! Testcase by G.Steinmetz

program p
  integer, parameter :: a((2.))   = [4,8] ! { dg-error "scalar INTEGER" }
  integer, parameter :: z(1:(2.)) = [4,8] ! { dg-error "scalar INTEGER" }
  print *, a(1:1)                         ! { dg-error "Syntax error" }
end
