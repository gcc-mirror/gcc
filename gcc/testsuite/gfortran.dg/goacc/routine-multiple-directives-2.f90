! Check for invalid (and some valid) cases of multiple OpenACC 'routine'
! directives.

      SUBROUTINE s_1
!$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(s_1)
!$ACC ROUTINE GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_1) SEQ
!$ACC ROUTINE
!$ACC ROUTINE(s_1) WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE s_1

      SUBROUTINE s_2
!$ACC ROUTINE(s_2) VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE
!$ACC ROUTINE(s_2) GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ
!$ACC ROUTINE(s_2)
!$ACC ROUTINE WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2) GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE s_2

      SUBROUTINE v_1
!$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE VECTOR
!$ACC ROUTINE GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_1) VECTOR
!$ACC ROUTINE WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE v_1

      SUBROUTINE v_2
!$ACC ROUTINE(v_2) VECTOR
!$ACC ROUTINE(v_2) VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE VECTOR
!$ACC ROUTINE(v_2) GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE v_2

      SUBROUTINE sub_1
      IMPLICIT NONE
      EXTERNAL :: g_1
!$ACC ROUTINE (g_1) GANG
!$ACC ROUTINE (g_1) GANG WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (g_1) VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) GANG
!$ACC ROUTINE (g_1) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }

      CALL s_1
      CALL s_2
      CALL v_1
      CALL v_2
      CALL g_1
      CALL ABORT
      END SUBROUTINE sub_1

      MODULE m_w_1
      IMPLICIT NONE
      EXTERNAL :: w_1
!$ACC ROUTINE (w_1) WORKER
!$ACC ROUTINE (w_1) WORKER SEQ ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (w_1) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) WORKER
!$ACC ROUTINE (w_1) SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }

      CONTAINS
      SUBROUTINE sub_2
      CALL s_1
      CALL s_2
      CALL v_1
      CALL v_2
      CALL w_1
      CALL ABORT
      END SUBROUTINE sub_2
      END MODULE m_w_1
