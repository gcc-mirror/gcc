! PR middle-end/99928
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
  implicit none
  integer :: r00, r01, r02

contains

subroutine bar ()
  integer :: i
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*in_reduction\\(\\+:r00\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*in_reduction\\(\\+:r00\\)" "gimple" } }
  !$omp master taskloop in_reduction(+:r00)
  do i = 1, 64
    r00 = r00 + 1
  end do
  ! { dg-final { scan-tree-dump-not "omp master\[^\n\r]*in_reduction\\(\\+:r01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*in_reduction\\(\\+:r01\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*in_reduction\\(\\+:r01\\)" "gimple" } }
  !$omp master taskloop simd in_reduction(+:r01)
  do i = 1, 64
    r01 = r01 + 1
  end do
  ! { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*in_reduction\\(\\+:r02\\)" "gimple" } }
  ! { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*in_reduction\\(\\+:r02\\)" "gimple" } }
  !$omp taskloop simd in_reduction(+:r02)
  do i = 1, 64
    r02 = r02 + 1
  end do
  ! FIXME: We don't support in_reduction clause on target yet, once we do, should
  ! add testcase coverage for all combined/composite constructs with target as leaf construct. 
end
end module m
