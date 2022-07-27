! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/104571 - ICE in resolve_elemental_actual
! Contributed by G.Steinmetz

program p
  real :: x(3)
  call g(x)                 ! { dg-error "Missing alternate return" }
contains
  elemental subroutine g(*) ! { dg-error "Alternate return specifier" }
  end
end
