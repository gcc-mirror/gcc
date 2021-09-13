! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: j00, j01, j02, j03, j04, j06, j07, j08, j09
  integer :: j10

contains

subroutine foo ()
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j00:1\\)" "gimple" } }
  !$omp distribute parallel do simd default(none)
  do j00 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j01:1\\)" "gimple" } }
  !$omp distribute simd
  do j01 = 1, 64
  end do
end

subroutine bar ()
  integer :: j05, j11, j12;
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j02:1\\)" "gimple" } }
  !$omp do simd
  do j02 = 1, 64
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j03\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j03\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j03\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j03:1\\)" "gimple" } }
  !$omp master taskloop simd default(none)
  do j03 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j04\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j04\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j04:1\\)" "gimple" } }
  !$omp parallel do simd default(none)
  do j04 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j05\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j05:1\\)" "gimple" } }
  !$omp parallel master taskloop simd default(none)
  do j05 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j06" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j06\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j06\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j06\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j06:1\\)" "gimple" } }
  !$omp target parallel do simd default(none) defaultmap(none)
  do j06 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j07" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j07\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j07:1\\)" "gimple" } }
  !$omp target simd defaultmap(none)
  do j07 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j08" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j08\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j08\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j08:1\\)" "gimple" } }
  !$omp target teams distribute parallel do simd default(none) defaultmap(none)
  do j08 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j09" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j09:1\\)" "gimple" } }
  !$omp target teams distribute simd default(none) defaultmap(none)
  do j09 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j10\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j10:1\\)" "gimple" } }
  !$omp taskloop simd default(none)
  do j10 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j11\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j11\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j11:1\\)" "gimple" } }
  !$omp teams distribute parallel do simd default(none)
  do j11 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j12\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j12\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j12:1\\)" "gimple" } }
  !$omp teams distribute simd default(none)
  do j12 = 1, 64
  end do
end
end module m
