! { dg-do link }
! { dg-options "-O1 -flto" }
! PR 117303

MODULE M1
  USE, INTRINSIC :: ISO_C_BINDING

  TYPE T
    TYPE(C_FUNPTR) FUNPTR
  END TYPE

  TYPE(T), POINTER :: T_POINTER

CONTAINS

  SUBROUTINE SET_FUNPTR(F)
    TYPE(C_FUNPTR), INTENT(IN) :: F
    T_POINTER%FUNPTR = F
  END SUBROUTINE

  SUBROUTINE S1() BIND(C)
  END SUBROUTINE

END MODULE

PROGRAM TEST
  USE M1
  INTEGER(C_INTPTR_T) I
  ALLOCATE(T_POINTER)
  ! There was no reference from TEST to S1 in the call graph,
  ! resulting in undefined-reference error with link-time optimization.
  CALL SET_FUNPTR(C_FUNLOC(S1))
  PRINT *, TRANSFER(T_POINTER%FUNPTR, I)
END PROGRAM
