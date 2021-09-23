! Check for valid cases of multiple OpenACC 'routine' directives.

! { dg-additional-options "-fdump-tree-oaccloops" }
!TODO See PR101551 for 'offloading_enabled' differences.

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

      SUBROUTINE s_1
!$ACC ROUTINE(s_1)
!$ACC ROUTINE(s_1) SEQ
!$ACC ROUTINE SEQ
      END SUBROUTINE s_1
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_1' doesn't have 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_1_' doesn't have 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE s_1_nh
!$ACC ROUTINE(s_1_nh) NOHOST
!$ACC ROUTINE(s_1_nh) SEQ NOHOST
!$ACC ROUTINE NOHOST SEQ
      END SUBROUTINE s_1_nh
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_1_nh' has 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_1_nh_' has 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE s_2
!$ACC ROUTINE
!$ACC ROUTINE SEQ
!$ACC ROUTINE(s_2)
      END SUBROUTINE s_2
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_2' doesn't have 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_2_' doesn't have 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE s_2_nh
!$ACC ROUTINE NOHOST
!$ACC ROUTINE NOHOST SEQ
!$ACC ROUTINE(s_2_nh) NOHOST
      END SUBROUTINE s_2_nh
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_2_nh' has 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 's_2_nh_' has 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE v_1
!$ACC ROUTINE VECTOR
!$ACC ROUTINE VECTOR
!$ACC ROUTINE(v_1) VECTOR
!$ACC ROUTINE VECTOR
! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-5 }
      END SUBROUTINE v_1
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_1' doesn't have 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_1_' doesn't have 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE v_1_nh
!$ACC ROUTINE NOHOST VECTOR
!$ACC ROUTINE VECTOR NOHOST
!$ACC ROUTINE(v_1_nh) NOHOST VECTOR
!$ACC ROUTINE VECTOR NOHOST
! { dg-bogus "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-5 }
      END SUBROUTINE v_1_nh
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_1_nh' has 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_1_nh_' has 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE v_2
!$ACC ROUTINE(v_2) VECTOR
!$ACC ROUTINE VECTOR
!$ACC ROUTINE(v_2) VECTOR
! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-4 }
      END SUBROUTINE v_2
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_2' doesn't have 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_2_' doesn't have 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE v_2_nh
!$ACC ROUTINE(v_2_nh) VECTOR NOHOST
!$ACC ROUTINE VECTOR NOHOST
!$ACC ROUTINE(v_2_nh) NOHOST VECTOR
! { dg-bogus "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-4 }
      END SUBROUTINE v_2_nh
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_2_nh' has 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
      ! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'v_2_nh_' has 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }

      SUBROUTINE sub_1
      IMPLICIT NONE
      EXTERNAL :: g_1
!$ACC ROUTINE (g_1) GANG
!$ACC ROUTINE (g_1) GANG
!$ACC ROUTINE (g_1) GANG
      EXTERNAL :: g_1_nh
!$ACC ROUTINE (g_1_nh) GANG NOHOST
!$ACC ROUTINE (g_1_nh) NOHOST GANG
!$ACC ROUTINE (g_1_nh) NOHOST GANG
!$ACC ROUTINE (g_1_nh) GANG NOHOST

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
!$ACC ROUTINE (w_1) WORKER
      EXTERNAL :: w_1_nh
!$ACC ROUTINE (w_1_nh) NOHOST WORKER
!$ACC ROUTINE (w_1_nh) WORKER NOHOST

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
