! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: f00, f01, f02, f03, f04, f05, f06, f07, f08, f09
  integer :: f12, f13, f14, f15, f16, f17, f18, f19
  integer :: f20, f21, f22, f23, f24, f25, f26, f27, f28, f29

contains

subroutine foo ()
  integer :: i
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*firstprivate\\(f00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f00\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f00\\)" "gimple" } } ! FIXME. 
  !$omp distribute parallel do firstprivate (f00) default(none)
  do i = 1, 64
    f00 = f00 + 1
  end do
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*firstprivate\\(f01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f01\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f01\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f01\\)" "gimple" } }
  !$omp distribute parallel do simd firstprivate (f01) default(none)
  do i = 1, 64
    f01 = f01 + 1
  end do
  ! { dg-final { scan-tree-dump "omp distribute\[^\n\r]*firstprivate\\(f02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f02\\)" "gimple" } }
  !$omp distribute simd firstprivate (f02)
  do i = 1, 64
    f02 = f02 + 1
  end do
end

subroutine bar ()
  integer :: f10, f11
  integer :: i
  f10 = 0; f11 = 0
  ! { dg-final { scan-tree-dump "omp for\[^\n\r]*firstprivate\\(f03\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f03\\)" "gimple" } }
  !$omp do simd firstprivate (f03)
  do i = 1, 64
    f03 = f03 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f04\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f04\\)" "gimple" } }
  !$omp master taskloop firstprivate (f04) default(none)
  do i = 1, 64
    f04 = f04 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f05\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f05\\)" "gimple" } }
  !$omp master taskloop simd firstprivate (f05) default(none)
  do i = 1, 64
    f05 = f05 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f06\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f06\\)" "gimple" } } ! FIXME. 
  !$omp parallel do firstprivate (f06) default(none)
  do i = 1, 64
    f06 = f06 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f07\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f07\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f07\\)" "gimple" } }
  !$omp parallel do simd firstprivate (f07) default(none)
  do i = 1, 64
    f07 = f07 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f08\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f08\\)" "gimple" } }
  !$omp parallel loop firstprivate (f08) default(none)
  do i = 1, 64
    f08 = f08 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f09\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f09\\)" "gimple" } }
  !$omp parallel master firstprivate (f09) default(none)
  f09 = f09 + 1
  !$omp end parallel master
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f10\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f10\\)" "gimple" } }
  !$omp parallel master taskloop firstprivate (f10) default(none)
  do i = 1, 64
    f10 = f10 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f11\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f11\\)" "gimple" } }
  !$omp parallel master taskloop simd firstprivate (f11) default(none)
  do i = 1, 64
    f11 = f11 + 1
  end do
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f12\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*firstprivate\\(f12\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp section \[^\n\r]*firstprivate\\(f12\\)" "gimple" } }
  !$omp parallel sections firstprivate (f12) default(none)
    f12 = f12 + 1
    !$omp section
    f12 = f12 + 1
  !$omp end parallel sections
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f13\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f13\\)" "gimple" } }
  !$omp target parallel firstprivate (f13) default(none) defaultmap(none)
  f13 = f13 + 1
  !$omp end target parallel
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f14\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f14\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f14\\)" "gimple" } } ! FIXME. 
  !$omp target parallel do firstprivate (f14) default(none) defaultmap(none)
  do i = 1, 64
    f14 = f14 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f15\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f15\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f15\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f15\\)" "gimple" } }
  !$omp target parallel do simd firstprivate (f15) default(none) defaultmap(none)
  do i = 1, 64
    f15 = f15 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f16\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f16\\)" "gimple" } }
  !$omp target parallel loop firstprivate (f16) default(none) defaultmap(none)
  do i = 1, 64
    f16 = f16 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f17\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f17\\)" "gimple" } }
  !$omp target teams firstprivate (f17) default(none) defaultmap(none)
  f17 = f17 + 1
  !$omp end target teams
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f18\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f18\\)" "gimple" } } ! FIXME: This should be on distribute instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f18\\)" "gimple" } } ! FIXME. 
  !$omp target teams distribute firstprivate (f18) default(none) defaultmap(none)
  do i = 1, 64
    f18 = f18 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f19\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } ! FIXME: This should be on distribute instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } ! FIXME. 
  !$omp target teams distribute parallel do firstprivate (f19) default(none) defaultmap(none)
  do i = 1, 64
    f19 = f19 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f20\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f20\\)" "gimple" } }
  !$omp target teams distribute parallel do simd firstprivate (f20) default(none) defaultmap(none)
  do i = 1, 64
    f20 = f20 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f21\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f21\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f21\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f21\\)" "gimple" } }
  !$omp target teams distribute simd firstprivate (f21) default(none) defaultmap(none)
  do i = 1, 64
    f21 = f21 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f22\\)" "gimple" } } ! NOTE: This is an implementation detail. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f22\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f22\\)" "gimple" } }
  !$omp target teams loop firstprivate (f22) default(none) defaultmap(none)
  do i = 1, 64
    f22 = f22 + 1
  end do
  ! { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f23\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f23\\)" "gimple" } }
  !$omp target simd firstprivate (f23) defaultmap(none)
  do i = 1, 64
    f23 = f23 + 1
  end do
  !$omp end target simd
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f24\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f24\\)" "gimple" } }
  !$omp taskloop simd firstprivate (f24) default(none)
  do i = 1, 64
    f24 = f24 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f25\\)" "gimple" } } ! FIXME: This should be on distribute instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f25\\)" "gimple" } } ! FIXME. 
  !$omp teams distribute firstprivate (f25) default(none)
  do i = 1, 64
    f25 = f25 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } ! FIXME: This should be on distribute instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } ! FIXME. 
  !$omp teams distribute parallel do firstprivate (f26) default(none)
  do i = 1, 64
    f26 = f26 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f27\\)" "gimple" } }
  !$omp teams distribute parallel do simd firstprivate (f27) default(none)
  do i = 1, 64
    f27 = f27 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f28\\)" "gimple" } } ! FIXME: This should be on for instead. 
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f28\\)" "gimple" } } ! FIXME. 
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f28\\)" "gimple" } }
  !$omp teams distribute simd firstprivate (f28) default(none)
  do i = 1, 64
    f28 = f28 + 1
  end do
  ! { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f29\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f29\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f29\\)" "gimple" } } ! NOTE: This is an implementation detail. 
  ! { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f29\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f29\\)" "gimple" } }
  !$omp teams loop firstprivate (f29) default(none)
  do i = 1, 64
    f29 = f29 + 1
  end do
end
end module m
