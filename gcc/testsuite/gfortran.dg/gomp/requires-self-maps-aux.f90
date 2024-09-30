! { dg-do compile  { target skip-all-targets } }

! used by requires-self-maps.f90

module m2
  implicit none
contains
  integer function g()
    !$omp target map(from:g)
       g = 99
    !$omp end target
  end
end

program main
  use m
  use m2
end
