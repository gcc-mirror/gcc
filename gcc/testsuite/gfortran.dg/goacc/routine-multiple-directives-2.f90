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
!$ACC ROUTINE VECTOR NOHOST WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(s_1) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_1) SEQ NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_1) NOHOST WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG NOHOST VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE s_1

      SUBROUTINE s_1_nh
!$ACC ROUTINE NOHOST VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(s_1_nh) NOHOST
!$ACC ROUTINE NOHOST GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_1_nh) NOHOST SEQ
!$ACC ROUTINE NOHOST
!$ACC ROUTINE(s_1_nh) WORKER NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG NOHOST VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(s_1_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_1_nh) SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_1_nh) WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE s_1_nh

      SUBROUTINE s_2
!$ACC ROUTINE(s_2) VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE
!$ACC ROUTINE(s_2) GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ
!$ACC ROUTINE(s_2)
!$ACC ROUTINE WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2) GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(s_2) VECTOR NOHOST WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2) GANG NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2) NOHOST GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE s_2

      SUBROUTINE s_2_nh
!$ACC ROUTINE(s_2_nh) NOHOST VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE NOHOST
!$ACC ROUTINE(s_2_nh) GANG NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ NOHOST
!$ACC ROUTINE(s_2_nh) NOHOST
!$ACC ROUTINE NOHOST WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2_nh) NOHOST GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(s_2_nh) VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2_nh) GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(s_2_nh) GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE s_2_nh

      SUBROUTINE v_1
!$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE VECTOR
!$ACC ROUTINE GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_1) VECTOR
!$ACC ROUTINE WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE NOHOST VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE NOHOST VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_1) VECTOR NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE WORKER NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG VECTOR NOHOST ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE v_1

      SUBROUTINE v_1_nh
!$ACC ROUTINE VECTOR WORKER NOHOST ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE VECTOR NOHOST
!$ACC ROUTINE GANG NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_1_nh) VECTOR NOHOST
!$ACC ROUTINE WORKER NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG NOHOST VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_1_nh) VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE v_1_nh

      SUBROUTINE v_2
!$ACC ROUTINE(v_2) VECTOR
!$ACC ROUTINE(v_2) VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE VECTOR
!$ACC ROUTINE(v_2) GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2) VECTOR NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_2) VECTOR NOHOST WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE VECTOR NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_2) NOHOST GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE v_2

      SUBROUTINE v_2_nh
!$ACC ROUTINE(v_2_nh) VECTOR NOHOST
!$ACC ROUTINE(v_2_nh) VECTOR WORKER NOHOST ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2_nh) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE VECTOR NOHOST
!$ACC ROUTINE(v_2_nh) GANG NOHOST VECTOR ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2_nh) VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_2_nh) VECTOR WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE(v_2_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE(v_2_nh) GANG VECTOR ! { dg-error "Multiple loop axes specified for routine" }
      END SUBROUTINE v_2_nh

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
!$ACC ROUTINE (g_1) NOHOST GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) GANG WORKER NOHOST ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (g_1) NOHOST VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) NOHOST SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) GANG NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
      EXTERNAL :: g_1_nh
!$ACC ROUTINE (g_1_nh) NOHOST GANG
!$ACC ROUTINE (g_1_nh) GANG NOHOST WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (g_1_nh) NOHOST VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) SEQ NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) GANG NOHOST
!$ACC ROUTINE (g_1_nh) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) GANG WORKER ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (g_1_nh) VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) GANG ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (g_1_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }

      CALL s_1
      CALL s_1_nh
      CALL s_2
      CALL s_2_nh
      CALL v_1
      CALL v_1_nh
      CALL v_2
      CALL v_2_nh
      CALL g_1
      CALL g_1_nh
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
!$ACC ROUTINE (w_1) WORKER NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) WORKER NOHOST SEQ ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (w_1) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) NOHOST WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) SEQ NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1) VECTOR NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
      EXTERNAL :: w_1_nh
!$ACC ROUTINE (w_1_nh) WORKER NOHOST
!$ACC ROUTINE (w_1_nh) WORKER NOHOST SEQ ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (w_1_nh) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) NOHOST WORKER
!$ACC ROUTINE (w_1_nh) NOHOST SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) VECTOR NOHOST ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) WORKER SEQ ! { dg-error "Multiple loop axes specified for routine" }
!$ACC ROUTINE (w_1_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) WORKER ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) SEQ ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) ! { dg-error "\\!\\\$ACC ROUTINE already applied" }
!$ACC ROUTINE (w_1_nh) VECTOR ! { dg-error "\\!\\\$ACC ROUTINE already applied" }

      CONTAINS
      SUBROUTINE sub_2
      CALL s_1
      CALL s_1_nh
      CALL s_2
      CALL s_2_nh
      CALL v_1
      CALL v_1_nh
      CALL v_2
      CALL v_2_nh
      CALL w_1
      CALL w_1_nh
      CALL ABORT
      END SUBROUTINE sub_2
      END MODULE m_w_1
