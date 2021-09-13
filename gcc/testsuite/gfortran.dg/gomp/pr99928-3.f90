! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: l00, l01, l02, l03, l04, l07, l08, l09
  integer :: l10, l11

contains

subroutine bar ()
  integer :: l05, l06
  integer :: i
  l05 = 0; l06 = 0
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*firstprivate\\(l00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(l00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l00\\)" "gimple" } }
  !$omp do simd firstprivate (l00) lastprivate (l00)
  do i = 1, 64
    l00 = i
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l01\\)" "gimple" } }
  !$omp master taskloop firstprivate (l01) lastprivate (l01) default(none)
  do i = 1, 64
    l01 = i
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l02\\)" "gimple" } }
  !$omp master taskloop simd firstprivate (l02) lastprivate (l02) default(none)
  do i = 1, 64
    l02 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } ! FIXME. 
  !$omp parallel do firstprivate (l03) lastprivate (l03) default(none)
  do i = 1, 64
    l03 = i
  end do
  !$omp end parallel do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l04\\)" "gimple" } }
  !$omp parallel do simd firstprivate (l04) lastprivate (l04) default(none)
  do i = 1, 64
    l04 = i
  end do
  !$omp end parallel do simd
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l05\\)" "gimple" } }
  !$omp parallel master taskloop firstprivate (l05) lastprivate (l05) default(none)
  do i = 1, 64
    l05 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l06\\)" "gimple" } }
  !$omp parallel master taskloop simd firstprivate (l06) lastprivate (l06) default(none)
  do i = 1, 64
    l06 = i
  end do
  !$omp end parallel master taskloop simd
  ! FIXME: OpenMP 5.0/5.1 broken here, conceptually it should be shared on parallel and
  ! firstprivate+lastprivate on sections, in GCC implementation we put firstprivate+lastprivate
  ! on parallel for historic reasons, but OpenMP 5.0/5.1 mistakenly say firstprivate
  ! should be on parallel and lastprivate on sections. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*firstprivate\\(l07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*lastprivate\\(l07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp section \[^\n\r]*firstprivate\\(l07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp section \[^\n\r]*lastprivate\\(l07\\)" "gimple" } }
  !$omp parallel sections firstprivate (l07) lastprivate (l07) default(none)
    l07 = 1
    !$omp section
    l07 = 2
  !$omp end parallel sections
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l08" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l08\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l08\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } ! FIXME. 
  !$omp target parallel do firstprivate (l08) lastprivate (l08) default(none) defaultmap(none)
  do i = 1, 64
    l08 = i
  end do
  !$omp end target parallel do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l09" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l09\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l09\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l09\\)" "gimple" } }
  !$omp target parallel do simd firstprivate (l09) lastprivate (l09) default(none) defaultmap(none)
  do i = 1, 64
    l09 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l10" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l10\\)" "gimple" } }
  !$omp target simd firstprivate (l10) lastprivate (l10) defaultmap(none)
  do i = 1, 64
    l10 = i
  end do
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l11\\)" "gimple" } }
 !$omp taskloop simd firstprivate (l11) lastprivate (l11) default(none)
  do i = 1, 64
    l11 = i
  end do
  !$omp end taskloop simd
end
end module m
