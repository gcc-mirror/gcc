! { dg-do compile }
! Structure constructor with component naming, test that an error is emitted
! if there are arguments without name after ones with name.

PROGRAM test
  IMPLICIT NONE

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i
    REAL :: r
  END TYPE basics_t

  TYPE(basics_t) :: basics

  basics = basics_t (i=42, 1.5) ! { dg-error "Missing keyword name" }

END PROGRAM test
