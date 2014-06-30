! { dg-do compile }
! { dg-options "-fopenmp -fopenmp-simd -fdump-tree-original -O2" }

include 'openmp-simd-1.f90'

! { dg-final { scan-tree-dump-times "pragma omp simd" 6 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp" 39 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp for" 6 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp parallel" 9 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp taskgroup" 1 "original" } }
! Includes the above taskgroup
! { dg-final { scan-tree-dump-times "pragma omp task" 3 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp critical" 1 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp atomic" 2 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp sections" 2 "original" } }
! Includes the above sections
! { dg-final { scan-tree-dump-times "pragma omp section" 6 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp single" 4 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp ordered" 1 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp master" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP" 5 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_barrier" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancellation_point" 1 "original" } }
! Includes the above cancellation point
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_taskyield" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_taskwait" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
