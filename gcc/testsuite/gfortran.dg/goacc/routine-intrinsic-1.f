! Check for valid clauses with intrinsic symbols specified in OpenACC
! 'routine' directives.

      SUBROUTINE sub_1
      IMPLICIT NONE
!$ACC ROUTINE (ABORT)
!$ACC ROUTINE (ABORT) SEQ

      CALL ABORT
      END SUBROUTINE sub_1

      MODULE m_w_1
      IMPLICIT NONE
!$ACC ROUTINE (ABORT) SEQ
!$ACC ROUTINE (ABORT)

      CONTAINS
      SUBROUTINE sub_2
      CALL ABORT
      END SUBROUTINE sub_2
      END MODULE m_w_1
