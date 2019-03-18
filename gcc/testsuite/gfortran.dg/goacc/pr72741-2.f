      SUBROUTINE v_1
!$ACC ROUTINE
!$ACC ROUTINE
!$ACC ROUTINE GANG ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ
!$ACC ROUTINE
!$ACC ROUTINE WORKER ! { dg-error "ACC ROUTINE already applied" }
      END SUBROUTINE v_1

      SUBROUTINE sub_1
      IMPLICIT NONE
      EXTERNAL :: g_1
!$ACC ROUTINE (g_1) GANG
!$ACC ROUTINE (g_1) VECTOR ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) SEQ ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) ! { dg-error "ACC ROUTINE already applied" }

      CALL v_1
      CALL g_1
      CALL ABORT
      END SUBROUTINE sub_1

      MODULE m_w_1
      IMPLICIT NONE
      EXTERNAL :: w_1
!$ACC ROUTINE (w_1) WORKER
!$ACC ROUTINE (w_1) ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) SEQ ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) ! { dg-error "ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) VECTOR ! { dg-error "ACC ROUTINE already applied" }

      CONTAINS
      SUBROUTINE sub_2
      CALL v_1
      CALL w_1
      CALL ABORT
      END SUBROUTINE sub_2
      END MODULE m_w_1
