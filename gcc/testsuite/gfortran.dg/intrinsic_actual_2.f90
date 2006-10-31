! { dg-do compile }
! Tests the fix for PR29387, in which array valued arguments of
! LEN and ASSOCIATED would cause an ICE.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
  TYPE T1 
    INTEGER, POINTER :: I=>NULL() 
  END TYPE T1 
  character(20) res

  j = 10
  PRINT *, LEN(SUB(8))
  PRINT *, LEN(SUB(j))
! print *, len(SUB(j + 2)//"a")   ! This still fails (no charlen).
  print *, len(bar(2))

  IF(.NOT.ASSOCIATED(F1(10))) CALL ABORT() 

CONTAINS

  FUNCTION SUB(I)  
    CHARACTER(LEN=I) :: SUB(1)
    PRINT *, LEN(SUB(1))
  END FUNCTION

  FUNCTION BAR(I)  
    CHARACTER(LEN=I*10) :: BAR(1)
    PRINT *, LEN(BAR)
  END FUNCTION

  FUNCTION F1(I) RESULT(R) 
   TYPE(T1), DIMENSION(:), POINTER :: R 
   INTEGER :: I 
   ALLOCATE(R(I)) 
  END FUNCTION F1 
END 
