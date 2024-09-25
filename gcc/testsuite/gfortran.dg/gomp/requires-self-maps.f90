! { dg-do link }
! { dg-additional-sources "requires-self-maps-aux.f90" }

module m
  !$omp requires self_maps
  implicit none
contains
  integer function f()
    !$omp target map(from:f)
       f = 42
    !$omp end target
  end
end

! This shows up in line 5 of requires-self-maps-aux.f90, but adding dg-error there is ignored when
! invoked as additional files – but this file needs to come first in order to create the .mod file:
! { dg-error "Program unit at .1. has OpenMP device constructs/routines but does not set !.OMP REQUIRES SELF_MAPS but other program units do" "" { target *-*-* } 0 }
