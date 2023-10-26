! { dg-additional-options "-fdump-tree-omplower" }
subroutine f()
  integer, allocatable :: A, B

  !$omp allocators allocate(align(128): A)
  allocate(A)

  !$omp allocate(B) align(256)
  allocate(B)
end

! { dg-final { scan-tree-dump "a = __builtin_GOMP_alloc \\(128, 4, 0B\\);" "omplower" } }
! { dg-final { scan-tree-dump "b = __builtin_GOMP_alloc \\(256, 4, 0B\\);" "omplower" } }
! { dg-final { scan-tree-dump "__builtin_GOMP_free \\(b, 0B\\);" "omplower" } }
! { dg-final { scan-tree-dump "__builtin_GOMP_free \\(a, 0B\\);" "omplower" } }
