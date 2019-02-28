! Check for multiple clauses specifying the level of parallelism.

SUBROUTINE v_1
  !$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
END SUBROUTINE v_1

SUBROUTINE sub_1
  IMPLICIT NONE
  EXTERNAL :: g_1
  !$ACC ROUTINE (g_1) GANG WORKER ! { dg-error "Multiple loop axes specified for routine" }
  !$ACC ROUTINE (ABORT) SEQ WORKER GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
  !$ACC ROUTINE WORKER SEQ ! { dg-error "Multiple loop axes specified for routine" }

  CALL v_1
  CALL g_1
  CALL ABORT
END SUBROUTINE sub_1

MODULE m_w_1
  IMPLICIT NONE
  EXTERNAL :: w_1
  !$ACC ROUTINE VECTOR GANG SEQ ! { dg-error "Multiple loop axes specified for routine" }
  !$ACC ROUTINE (w_1) GANG WORKER SEQ ! { dg-error "Multiple loop axes specified for routine" }
  !$ACC ROUTINE (ABORT) VECTOR GANG ! { dg-error "Multiple loop axes specified for routine" }

CONTAINS
  SUBROUTINE sub_2
    CALL v_1
    CALL w_1
    CALL ABORT
  END SUBROUTINE sub_2
END MODULE m_w_1
