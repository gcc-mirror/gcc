! { dg-do compile }
! PR46356 - class arrays 
!
! Contributed by Ian Harvey
!
MODULE procedure_intent_nonsense
  IMPLICIT NONE  
  PRIVATE    
  TYPE, PUBLIC :: Parent
    INTEGER :: comp
  END TYPE Parent

  TYPE :: ParentVector
    INTEGER :: a
    ! CLASS(Parent), ALLOCATABLE :: a
  END TYPE ParentVector  
CONTAINS           
  SUBROUTINE vector_operation(pvec)     
    CLASS(ParentVector), INTENT(INOUT) :: pvec(:)
    INTEGER :: i    
    !---
    DO i = 1, SIZE(pvec)
      CALL item_operation(pvec(i))
    END DO  
    ! PRINT *, pvec(1)%a%comp
  END SUBROUTINE vector_operation

  SUBROUTINE item_operation(pvec)  
    CLASS(ParentVector), INTENT(INOUT) :: pvec
    !TYPE(ParentVector), INTENT(INOUT) :: pvec
  END SUBROUTINE item_operation
END MODULE procedure_intent_nonsense
! { dg-final { cleanup-modules "procedure_intent_nonsense" } }
