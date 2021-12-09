! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump-times "omp atomic release" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp atomic relaxed" 3 "original" } }
! { dg-final { scan-tree-dump-times "omp atomic read relaxed" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp atomic capture relaxed" 1 "original" } }

module mod
  implicit none
  integer i, j, k, l, m, n

contains

subroutine foo ()
  !$omp atomic release
  i = i + 1;
end
end

module m2
use mod
implicit none
!$omp requires atomic_default_mem_order (relaxed)

contains
subroutine bar ()
  integer v;
  !$omp atomic
  j = j + 1
  !$omp atomic update
  k = k + 1
  !$omp atomic read
  v = l
  !$omp atomic write
  m = v
  !$omp atomic capture
  n = n + 1; v = n
end
end module m2
