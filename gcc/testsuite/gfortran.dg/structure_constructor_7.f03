! { dg-do compile }
! Test for errors when excess components are given for a structure-constructor.

PROGRAM test
  IMPLICIT NONE

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i
    REAL :: r = 1.5
  END TYPE basics_t

  TYPE(basics_t) :: basics

  basics = basics_t (42, 1.5, 1000) ! { dg-error "Too many components" }
  basics = basics_t (42, xxx = 1000) ! { dg-error "is not a member" }

END PROGRAM test
