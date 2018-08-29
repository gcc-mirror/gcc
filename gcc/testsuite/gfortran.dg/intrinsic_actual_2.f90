! { dg-do compile }
! Tests the fix for PR29387, in which array valued arguments of
! LEN and ASSOCIATED would cause an ICE.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
  integer  :: ans
  TYPE T1 
    INTEGER, POINTER :: I=>NULL() 
  END TYPE T1
  type(T1), pointer :: tar(:)
 
  character(20) res

  j = 10
  PRINT *, LEN(SUB(8)), ans
  PRINT *, LEN(SUB(j)), ans
!  print *, len(SUB(j + 2)//"a"), ans   ! This still fails (no charlen).
  print *, len(bar(2)), ans

  IF(.NOT.ASSOCIATED(F1(10))) STOP 1
  deallocate (tar)

CONTAINS

  FUNCTION SUB(I)  
    CHARACTER(LEN=I) :: SUB(1)
    ans = LEN(SUB(1))
    SUB = ""
  END FUNCTION

  FUNCTION BAR(I)  
    CHARACTER(LEN=I*10) :: BAR(1)
    ans = LEN(BAR)
    BAR = ""
  END FUNCTION

  FUNCTION F1(I) RESULT(R) 
   TYPE(T1), DIMENSION(:), POINTER :: R 
   INTEGER :: I 
   ALLOCATE(tar(I))
   R => tar 
  END FUNCTION F1
END 
