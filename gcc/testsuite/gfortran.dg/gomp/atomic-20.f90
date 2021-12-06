! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump-times "omp atomic release" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp atomic seq_cst" 3 "original" } }
! { dg-final { scan-tree-dump-times "omp atomic read seq_cst" 1 "original" } }
! { dg-final { scan-tree-dump-times "omp atomic capture seq_cst" 1 "original" } }

module mod
implicit none
integer i, j, k, l, m, n

contains 
subroutine foo ()
  !$omp atomic release
  i = i + 1
end
end module

module m2
use mod
implicit none
!$omp requires atomic_default_mem_order (seq_cst)

contains

subroutine bar ()
  integer v
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
end module
