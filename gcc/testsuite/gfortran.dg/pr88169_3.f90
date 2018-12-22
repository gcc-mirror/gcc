! { dg-do compile }
! { dg-options "-std=f95" }
module foo_nml
   implicit none
   real :: x = -1
   namelist /foo/ x
end module

program main
   use foo_nml, only: bar => foo, x
   implicit none
   real a
   namelist /bar/a  ! { dg-error "already is USE associated" }
end program
! { dg-final { cleanup-modules "foo_nml" } }
