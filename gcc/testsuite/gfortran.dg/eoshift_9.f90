! { dg-do compile }
! PR fortran/104331 - ICE in gfc_simplify_eoshift
! Contributed by G.Steinmetz

program p
  character(3), parameter :: a(:) = ['123'] ! { dg-error "deferred shape" }
  character(3), parameter :: b(*) = eoshift(a, 1)
end
