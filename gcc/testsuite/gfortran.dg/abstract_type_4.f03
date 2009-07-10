! { dg-do "compile" }

! Abstract Types.
! Check for module file IO.

MODULE m
  IMPLICIT NONE

  TYPE, ABSTRACT :: abst_t
    INTEGER :: x
  END TYPE abst_t

  TYPE, EXTENDS(abst_t) :: concrete_t
    INTEGER :: y
  END TYPE concrete_t

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE

  TYPE(abst_t) :: abst ! { dg-error "is of the ABSTRACT type 'abst_t'" }
  TYPE(concrete_t) :: conc

  ! See if constructing the extending type works.
  conc = concrete_t (1, 2)
END PROGRAM main
! { dg-final { cleanup-modules "m" } }
