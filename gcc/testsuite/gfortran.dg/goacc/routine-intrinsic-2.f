! Check for invalid clauses with intrinsic symbols specified in OpenACC
! 'routine' directives.

      SUBROUTINE sub_1
      IMPLICIT NONE
!$ACC ROUTINE (ABORT) WORKER ! { dg-error "Intrinsic symbol specified in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\) marked with incompatible GANG, WORKER, or VECTOR clause" }
!$ACC ROUTINE (ABORT) GANG ! { dg-error "Intrinsic symbol specified in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\) marked with incompatible GANG, WORKER, or VECTOR clause" }
!$ACC ROUTINE (ABORT) VECTOR ! { dg-error "Intrinsic symbol specified in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\) marked with incompatible GANG, WORKER, or VECTOR clause" }

      CALL ABORT
      END SUBROUTINE sub_1

      MODULE m_w_1
      IMPLICIT NONE
!$ACC ROUTINE (ABORT) VECTOR ! { dg-error "Intrinsic symbol specified in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\) marked with incompatible GANG, WORKER, or VECTOR clause" }
!$ACC ROUTINE (ABORT) WORKER ! { dg-error "Intrinsic symbol specified in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\) marked with incompatible GANG, WORKER, or VECTOR clause" }
!$ACC ROUTINE (ABORT) GANG ! { dg-error "Intrinsic symbol specified in \\!\\\$ACC ROUTINE \\( NAME \\) at \\(1\\) marked with incompatible GANG, WORKER, or VECTOR clause" }

      CONTAINS
      SUBROUTINE sub_2
      CALL ABORT
      END SUBROUTINE sub_2
      END MODULE m_w_1
