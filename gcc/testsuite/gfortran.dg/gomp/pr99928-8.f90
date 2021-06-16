! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: r00, r01, r02, r03, r04, r05
  integer :: r13, r14, r15, r16, r17, r18, r19
  integer :: r20, r21, r22, r23, r24

contains

subroutine foo ()
  integer :: i
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r00\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r00\\)" "gimple" } } ! FIXME. 
  !$omp distribute parallel do reduction(+:r00) default(none)
  do i = 1, 64
    r00 = r00 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } }
  !$omp distribute parallel do simd reduction(+:r01) default(none)
  do i = 1, 64
    r01 = r01 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r02\\)" "gimple" } }
  !$omp distribute simd reduction(+:r02)
  do i = 1, 64
    r02 = r02 + 1
  end do
end

subroutine bar ()
  integer :: r06, r07, r08, r09
  integer :: r10, r11, r12
  integer :: r25, r26, r27, r28, r29
  integer :: i
  r06 = 0; r07 = 0; r08 = 0; r09 = 0
  r10 = 0; r11 = 0; r12 = 0
  r25 = 0; r26 = 0; r27 = 0; r28 = 0; r29 = 0
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r03\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r03\\)" "gimple" } }
  !$omp do simd reduction(+:r03)
  do i = 1, 64
    r03 = r03 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r04\\)" "gimple" } }
  !$omp master taskloop reduction(+:r04) default(none)
  do i = 1, 64
    r04 = r04 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r05\\)" "gimple" } }
  !$omp master taskloop simd reduction(+:r05) default(none)
  do i = 1, 64
    r05 = r05 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r06\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r06\\)" "gimple" } } ! FIXME. 
  !$omp parallel do reduction(+:r06) default(none)
  do i = 1, 64
    r06 = r06 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r07\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r07\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r07\\)" "gimple" } }
  !$omp parallel do simd reduction(+:r07) default(none)
  do i = 1, 64
    r07 = r07 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r08\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r08\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp parallel loop reduction(+:r08) default(none)
  do i = 1, 64
    r08 = r08 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r09\\)" "gimple" } }
  !$omp parallel master reduction(+:r09) default(none)
  r09 = r09 + 1
  !$omp end parallel master
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r10\\)" "gimple" } }
  !$omp parallel master taskloop reduction(+:r10) default(none)
  do i = 1, 64
    r10 = r10 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r11\\)" "gimple" } }
  !$omp parallel master taskloop simd reduction(+:r11) default(none)
  do i = 1, 64
    r11 = r11 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r12\\)" "gimple" } } ! FIXME: This should be on sections instead. 
  ! { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*reduction\\(\\+:r12\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp section \[^\n\r]*reduction\\(\\+:r12\\)" "gimple" } }
  !$omp parallel sections reduction(+:r12) default(none)
    r12 = r12 + 1
    !$omp section
    r12 = r12 + 1
  !$omp end parallel sections
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r13" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r13\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r13\\)" "gimple" } }
  !$omp target parallel reduction(+:r13) default(none) defaultmap(none)
  r13 = r13 + 1
  !$omp end target parallel
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r14" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r14\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r14\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r14\\)" "gimple" } } ! FIXME. 
  !$omp target parallel do reduction(+:r14) default(none) defaultmap(none)
  do i = 1, 64
    r14 = r14 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r15" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r15\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r15\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r15\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r15\\)" "gimple" } }
  !$omp target parallel do simd reduction(+:r15) default(none) defaultmap(none)
  do i = 1, 64
    r15 = r15 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r16" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r16\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r16\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp target parallel loop reduction(+:r16) default(none) defaultmap(none)
  do i = 1, 64
    r16 = r16 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r17" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r17\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r17\\)" "gimple" } }
  !$omp target teams reduction(+:r17) default(none) defaultmap(none)
  r17 = r17 + 1
  !$omp end target teams
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r18" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r18\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r18\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r18\\)" "gimple" } }
  !$omp target teams distribute reduction(+:r18) default(none) defaultmap(none)
  do i = 1, 64
    r18 = r18 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r19" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r19\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } } ! FIXME. 
  !$omp target teams distribute parallel do reduction(+:r19) default(none) defaultmap(none)
  do i = 1, 64
    r19 = r19 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r20" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r20\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } }
  !$omp target teams distribute parallel do simd reduction(+:r20) default(none) defaultmap(none)
  do i = 1, 64
    r20 = r20 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r21" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r21\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r21\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r21\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r21\\)" "gimple" } }
  !$omp target teams distribute simd reduction(+:r21) default(none) defaultmap(none)
  do i = 1, 64
    r21 = r21 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r22" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(r22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*reduction\\(\\+:r22\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r22\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r22\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r22\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp target teams loop reduction(+:r22) default(none) defaultmap(none)
  do i = 1, 64
    r22 = r22 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r23" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r23\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r23\\)" "gimple" } }
  !$omp target simd reduction(+:r23) defaultmap(none)
  do i = 1, 64
    r23 = r23 + 1
  end do
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r24\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r24\\)" "gimple" } }
  !$omp taskloop simd reduction(+:r24) default(none)
  do i = 1, 64
    r24 = r24 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r25\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r25\\)" "gimple" } }
  !$omp teams distribute reduction(+:r25) default(none)
  do i = 1, 64
    r25 = r25 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } } ! FIXME. 
  !$omp teams distribute parallel do reduction(+:r26) default(none)
  do i = 1, 64
    r26 = r26 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } }
  !$omp teams distribute parallel do simd reduction(+:r27) default(none)
  do i = 1, 64
    r27 = r27 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r28\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r28\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r28\\)" "gimple" } }
  !$omp teams distribute simd reduction(+:r28) default(none)
  do i = 1, 64
    r28 = r28 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(r29\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*reduction\\(\\+:r29\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r29\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r29\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r29\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp teams loop reduction(+:r29) default(none)
  do i = 1, 64
    r29 = r29 + 1
  end do
end
end module m
