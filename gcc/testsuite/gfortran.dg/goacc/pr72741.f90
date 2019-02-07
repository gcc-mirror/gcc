SUBROUTINE v_1
  !$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes" }
END SUBROUTINE v_1

SUBROUTINE sub_1
  IMPLICIT NONE
  EXTERNAL :: g_1
  !$ACC ROUTINE (g_1) GANG WORKER ! { dg-error "Multiple loop axes" }
  !$ACC ROUTINE (ABORT) SEQ VECTOR ! { dg-error "Multiple loop axes" "" { xfail *-*-* } }
! { dg-bogus "invalid function name abort" "" { xfail *-*-* } .-1 }

  CALL v_1
  CALL g_1
  CALL ABORT
END SUBROUTINE sub_1

MODULE m_w_1
  IMPLICIT NONE
  EXTERNAL :: w_1
  !$ACC ROUTINE (w_1) WORKER SEQ ! { dg-error "Multiple loop axes" }
  !$ACC ROUTINE (ABORT) VECTOR GANG ! { dg-error "Multiple loop axes" "" { xfail *-*-* } }
! { dg-bogus "invalid function name abort" "" { xfail *-*-* } .-1 }

CONTAINS
  SUBROUTINE sub_2
    CALL v_1
    CALL w_1
    CALL ABORT
  END SUBROUTINE sub_2
END MODULE m_w_1
