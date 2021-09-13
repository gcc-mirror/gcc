! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: l00, l01, l02, l03, l04, l05, l06, l07
  integer :: l10, l11, l12, l13, l14, l15, l16, l17, l18

contains

subroutine foo ()
  integer :: i
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } ! FIXME. 
  !$omp distribute parallel do lastprivate (l00) default(none)
  do i = 1, 64
    l00 = i
  end do
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l01\\)" "gimple" } }
  !$omp distribute parallel do simd lastprivate (l01) default(none)
  do i = 1, 64
    l01 = i
  end do
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l02\\)" "gimple" } }
  !$omp distribute simd lastprivate (l02)
  do i = 1, 64
    l02 = i
  end do
end

subroutine bar ()
  integer :: j00, j01, j02, j03
  integer :: l08, l09, l19, l20, l21, l22
  integer :: i
  l08 = 0; l09 = 0; l19 = 0; l20 = 0; l21 = 0; l22 = 0
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(l03\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l03\\)" "gimple" } }
  !$omp do simd lastprivate (l03)
  do i = 1, 64
    l03 = i
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l04\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l04\\)" "gimple" } }
  !$omp master taskloop lastprivate (l04) default(none)
  do i = 1, 64
    l04 = i
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l05\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l05\\)" "gimple" } }
  !$omp master taskloop simd lastprivate (l05) default(none)
  do i = 1, 64
    l05 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } ! FIXME. 
  !$omp parallel do lastprivate (l06) default(none)
  do i = 1, 64
    l06 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l07\\)" "gimple" } }
  !$omp parallel do simd lastprivate (l07) default(none)
  do i = 1, 64
    l07 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp parallel loop lastprivate (j00) default(none)
  do j00 = 1, 64
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l08\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l08\\)" "gimple" } }
  !$omp parallel master taskloop lastprivate (l08) default(none)
  do i = 1, 64
    l08 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l09\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l09\\)" "gimple" } }
  !$omp parallel master taskloop simd lastprivate (l09) default(none)
  do i = 1, 64
    l09 = i
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l10\\)" "gimple" } } ! FIXME: This should be on sections instead. 
  ! { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*lastprivate\\(l10\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp section \[^\n\r]*lastprivate\\(l10\\)" "gimple" } }
  !$omp parallel sections lastprivate (l10) default(none)
    l10 = 1
    !$omp section
    l10 = 2
  !$omp end parallel sections
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l11" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l11\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l11\\)" "gimple" } } ! FIXME. 
  !$omp target parallel do lastprivate (l11) default(none) defaultmap(none)
  do i = 1, 64
    l11 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l12" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l12\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l12\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l12\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l12\\)" "gimple" } }
  !$omp target parallel do simd lastprivate (l12) default(none) defaultmap(none)
  do i = 1, 64
    l12 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j01" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j01\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j01\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp target parallel loop lastprivate (j01) default(none) defaultmap(none)
  do j01 = 0, 64
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l13" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l13\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l13\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l13\\)" "gimple" } }
  !$omp target teams distribute lastprivate (l13) default(none) defaultmap(none)
  do i = 1, 64
    l13 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l14" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l14\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l14\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l14\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l14\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l14\\)" "gimple" } } ! FIXME. 
  !$omp target teams distribute parallel do lastprivate (l14) default(none) defaultmap(none)
  do i = 1, 64
    l14 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l15" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l15\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l15\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l15\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l15\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l15\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l15\\)" "gimple" } }
  !$omp target teams distribute parallel do simd lastprivate (l15) default(none) defaultmap(none)
  do i = 1, 64
    l15 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l16" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l16\\)" "gimple" } }
  !$omp target teams distribute simd lastprivate (l16) default(none) defaultmap(none)
  do i = 1, 64
    l16 = i
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j02" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j02\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp target teams loop lastprivate (j02) default(none) defaultmap(none)
  do j02 = 0, 64
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l17" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l17\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l17\\)" "gimple" } }
  !$omp target simd lastprivate (l17) defaultmap(none)
  do i = 1, 64
    l17 = i
  end do
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l18\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l18\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l18\\)" "gimple" } }
  !$omp taskloop simd lastprivate (l18) default(none)
  do i = 1, 64
    l18 = i
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l19\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l19\\)" "gimple" } }
  !$omp teams distribute lastprivate (l19) default(none)
  do i = 1, 64
    l19 = i
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l20\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l20\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l20\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l20\\)" "gimple" } } ! FIXME. 
 !$omp teams distribute parallel do lastprivate (l20) default(none)
  do i = 1, 64
    l20 = i
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l21\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l21\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l21\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l21\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l21\\)" "gimple" } }
 !$omp teams distribute parallel do simd lastprivate (l21) default(none)
  do i = 1, 64
    l21 = i
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l22\\)" "gimple" } }
  !$omp teams distribute simd lastprivate (l22) default(none)
  do i = 1, 64
    l22 = i
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j03\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j03\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } ! NOTE: This is implementation detail. 
  ! { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } ! NOTE: This is implementation detail. 
  !$omp teams loop lastprivate (j03) default(none)
  do j03 = 1, 64
  end do
end
end module m
