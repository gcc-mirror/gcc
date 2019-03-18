      SUBROUTINE sub_1
      IMPLICIT NONE
!$ACC ROUTINE (ABORT) SEQ VECTOR ! { dg-error "Multiple loop axes specified for routine" }

      CALL ABORT
      END SUBROUTINE sub_1

      MODULE m_w_1
      IMPLICIT NONE
!$ACC ROUTINE (ABORT) VECTOR GANG ! { dg-error "Multiple loop axes specified for routine" }

      CONTAINS
      SUBROUTINE sub_2
      CALL ABORT
      END SUBROUTINE sub_2
      END MODULE m_w_1
