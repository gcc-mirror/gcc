! { dg-do compile }
! PR fortran/106985 - ICE in gfc_simplify_expr
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(2) = 1
  integer, parameter :: b = a(2) + b ! { dg-error "before its definition is complete" }
end
