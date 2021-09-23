! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: l00, l01, l05, l06, l07, l08

contains

subroutine bar ()
  integer :: l02, l03, l04
  integer :: i
  l02 = 0; l03 = 0; l04 = 0
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*firstprivate\\(l00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(l00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l00:1\\)" "gimple" } }
  !$omp do simd linear (l00)
  do i = 1, 64
    l00 = l00 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l01:1\\)" "gimple" } }
  !$omp master taskloop simd linear (l01) default(none)
  do i = 1, 64
    l01 = l01 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*linear\\(l02:1\\)" "gimple" } }
  !$omp parallel do linear (l02) default(none)
  do i = 1, 64
    l02 = l02 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l03:1\\)" "gimple" } }
  !$omp parallel do simd linear (l03) default(none)
  do i = 1, 64
    l03 = l03 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l04:1\\)" "gimple" } }
  !$omp parallel master taskloop simd linear (l04) default(none)
  do i = 1, 64
    l04 = l04 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l05" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*linear\\(l05:1\\)" "gimple" } }
  !$omp target parallel do linear (l05) default(none) defaultmap(none)
  do i = 1, 64
    l05 = l05 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l06" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l06:1\\)" "gimple" } }
  !$omp target parallel do simd linear (l06) default(none) defaultmap(none)
  do i = 1, 64
    l06 = l06 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l07" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l07:1\\)" "gimple" } }
  !$omp target simd linear (l07) defaultmap(none)
  do i = 1, 64
    l07 = l07 + 1
  end do
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l08:1\\)" "gimple" } }
  !$omp taskloop simd linear (l08) default(none)
  do i = 1, 64
    l08 = l08 + 1
  end do
end
end module m
