! { dg-additional-options "-fdump-tree-original" }
!$omp taskwait
!$omp taskwait depend(out:foo)
end

! { dg-final { scan-tree-dump-times "__builtin_GOMP_taskwait \\(\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp taskwait depend\\(out:foo\\)" 1 "original" } }
