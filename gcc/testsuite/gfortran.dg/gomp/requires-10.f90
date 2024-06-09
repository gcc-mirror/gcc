! { dg-additional-options "-fdump-tree-original" }

function foo (x, y) result (z)
  !$omp requires atomic_default_mem_order(release)
  implicit none
  real :: x, y, z

  !$omp atomic write
    x = y

  !$omp atomic update
    x = x + 1

  !$omp atomic read acquire
    z = x
end

function bar (a, b) result (c)
  !$omp requires atomic_default_mem_order(acquire)
  implicit none
  real :: a, b, c

  !$omp atomic write release
    a = b

  !$omp atomic update
    a = a + 1

  !$omp atomic read
    c = a
end

! { dg-final { scan-tree-dump-times "#pragma omp atomic release" 3 "original" } } */
! { dg-final { scan-tree-dump-times "#pragma omp atomic acquire" 1 "original" } } */
! { dg-final { scan-tree-dump-times "z = #pragma omp atomic read acquire" 1 "original" } } */
! { dg-final { scan-tree-dump-times "c = #pragma omp atomic read acquire" 1 "original" } } */
