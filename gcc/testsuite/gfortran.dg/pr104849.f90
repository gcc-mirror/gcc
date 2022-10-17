! { dg-do compile }
! PR fortran/104849 - ICE in find_array_section
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(:) = [1, 2] ! { dg-error "deferred shape" }
  integer :: x(2)
  data x /a(:)/                       ! { dg-error "Invalid" }
end
