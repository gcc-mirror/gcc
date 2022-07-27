! { dg-do compile }
! PR fortran/105230 - ICE in find_array_section
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(:) = [1, 2] ! { dg-error "deferred shape" }
  print *, reshape([3, 4], a(1:2))
end
