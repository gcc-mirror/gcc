! { dg-do compile }
! PR fortran/106209 - ICE in add_init_expr_to_sym
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(:) = 0   ! { dg-error "of deferred shape" }
  integer, parameter :: b(*) = a   ! { dg-error "Bad shape of initializer" }
  integer, parameter :: c(*) = [a] ! { dg-error "Cannot determine shape" }
end
