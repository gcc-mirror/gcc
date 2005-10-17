! { dg-do compile }
! PR20837 - A symbol may not be declared PUBLIC or PRIVATE outside a module.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
integer, parameter, public :: i=1 ! { dg-error "allowed outside of a MODULE" }
END
