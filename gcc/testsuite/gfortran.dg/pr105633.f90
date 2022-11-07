! { dg-do compile }
! PR fortran/105633 - ICE in find_array_section
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(:) = [1,2] ! { dg-error "deferred shape" }
  print *, [a([1,2])]
end
