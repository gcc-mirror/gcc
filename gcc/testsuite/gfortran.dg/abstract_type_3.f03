! { dg-do "compile" }

! Abstract Types.
! Check for errors when using abstract types in an inappropriate way.

MODULE m
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE, ABSTRACT, BIND(C) :: bindc_t ! { dg-error "must not be ABSTRACT" }
    INTEGER(C_INT) :: x
  END TYPE bindc_t

  TYPE, ABSTRACT :: sequence_t ! { dg-error "must not be ABSTRACT" }
    SEQUENCE
    INTEGER :: x
  END TYPE sequence_t

  TYPE, ABSTRACT :: abst_t
    INTEGER :: x = 0
  END TYPE abst_t

  TYPE, EXTENDS(abst_t) :: concrete_t
    INTEGER :: y = 1
  END TYPE concrete_t

  TYPE :: myt
    TYPE(abst_t) :: comp ! { dg-error "is of the ABSTRACT type 'abst_t'" }
  END TYPE myt

  ! This should be ok.
  TYPE, ABSTRACT, EXTENDS(concrete_t) :: again_abst_t
    INTEGER :: z = 2
  END TYPE again_abst_t

CONTAINS

  TYPE(abst_t) FUNCTION func () ! { dg-error "of the ABSTRACT type 'abst_t'" }
  END FUNCTION func

  SUBROUTINE sub (arg) ! { dg-error "is of the ABSTRACT type 'again_abst_t'" }
    IMPLICIT NONE
    TYPE(again_abst_t) :: arg
    arg = again_abst_t () ! { dg-error "Can't construct ABSTRACT type 'again_abst_t'" }
  END SUBROUTINE sub

  SUBROUTINE impl ()
    IMPLICIT TYPE(abst_t) (a-z) ! { dg-error "ABSTRACT type 'abst_t' used" }
  END SUBROUTINE impl

END MODULE m
