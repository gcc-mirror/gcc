! { dg-do compile }
! Structure constructor with default initialization, test that an error is
! emitted for components without default initializer missing value.

PROGRAM test
  IMPLICIT NONE

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i = 42
    REAL :: r
    COMPLEX :: c = (0., 1.)
  END TYPE basics_t

  TYPE(basics_t) :: basics

  basics = basics_t (i = 42) ! { dg-error "No initializer for component 'r'" }
  basics = basics_t (42) ! { dg-error "No initializer for component 'r'" }

END PROGRAM test
