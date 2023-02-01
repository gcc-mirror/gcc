! { dg-do compile }
! PR fortran/108527 - ICE in compare_bound_int
! Contributed by G.Steinmetz

program p
  integer, parameter :: a((2.)) = [4,8] ! { dg-error "must be of INTEGER type" }
  integer(a(1:1)) :: b                  ! { dg-error "Unclassifiable statement" }
end

! { dg-prune-output "Parameter array" }
