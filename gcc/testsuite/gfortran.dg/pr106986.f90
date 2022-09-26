! { dg-do compile }
! PR fortran/106986 - ICE in simplify_findloc_nodim
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(:) = [1] ! { dg-error "deferred shape" }
  print *, findloc (a, 1)
end
