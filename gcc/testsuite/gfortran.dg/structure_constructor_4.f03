! { dg-do compile }
! Structure constructor with component naming, test that an error is emitted if
! a component is given two initializers.

PROGRAM test
  IMPLICIT NONE

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i
    REAL :: r
  END TYPE basics_t

  TYPE(basics_t) :: basics

  basics = basics_t (42, r=1.5, i=15) ! { dg-error "'i' is initialized twice" }
  basics = basics_t (42, r=1., r=-2.) ! { dg-error "'r' is initialized twice" }

END PROGRAM test
