! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR40443 in which the final call to the generic
! 'SpecElem' was resolved to the elemental rather than the specific
! procedure, which is required by the second part of 12.4.4.1.
!
! Contributed by Ian Harvey <ian_harvey@bigpond.com>
!
MODULE SomeOptions
  IMPLICIT NONE  
  INTERFACE ElemSpec
    MODULE PROCEDURE ElemProc
    MODULE PROCEDURE SpecProc
  END INTERFACE ElemSpec  
  INTERFACE SpecElem
    MODULE PROCEDURE SpecProc
    MODULE PROCEDURE ElemProc
  END INTERFACE SpecElem
CONTAINS
  ELEMENTAL SUBROUTINE ElemProc(a)  
    CHARACTER, INTENT(OUT) :: a
    !****
    a = 'E'            
  END SUBROUTINE ElemProc

  SUBROUTINE SpecProc(a)  
    CHARACTER, INTENT(OUT) :: a(:)
    !****    
    a = 'S'    
  END SUBROUTINE SpecProc
END MODULE SomeOptions

PROGRAM MakeAChoice
  USE SomeOptions  
  IMPLICIT NONE
  CHARACTER scalar, array(2)    
  !****
  CALL ElemSpec(scalar) ! Should choose the elemental (and does)
  WRITE (*, 100) scalar
  CALL ElemSpec(array)  ! Should choose the specific (and does)
  WRITE (*, 100) array
  !----
  CALL SpecElem(scalar) ! Should choose the elemental (and does)
  WRITE (*, 100) scalar
  CALL SpecElem(array)  ! Should choose the specific (but didn't)
  WRITE (*, 100) array  
  !----
  100 FORMAT(A,:,', ',A)
END PROGRAM MakeAChoice
! { dg-final { scan-tree-dump-times "specproc" 3 "original" } }
! { dg-final { scan-tree-dump-times "elemproc" 3 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
