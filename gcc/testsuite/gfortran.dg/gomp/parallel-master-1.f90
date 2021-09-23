! { dg-additional-options "-fdump-tree-original" }
  implicit none
  integer :: k, p, s, r, nth, t, f
  logical(kind=2) l2
  !$omp threadprivate (t)
  
  external bar
  !$omp parallel master default(none) private (k)
    call bar (k)
  !$omp end parallel master

  !$omp parallel master private (p) firstprivate (f) if (parallel: l2) default(shared) &
  !$omp& shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) copyin(t)
     !
  !$omp end parallel master
end

! { dg-final { scan-tree-dump "omp parallel private\\(k\\) default\\(none\\)" "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp master" 1 "original" } }
! { dg-final { scan-tree-dump "D.\[0-9\]+ = l2;" "original" } }
! { dg-final { scan-tree-dump "D.\[0-9\]+ = nth;" "original" } }
! { dg-final { scan-tree-dump "#pragma omp parallel private\\(p\\) firstprivate\\(f\\) shared\\(s\\) copyin\\(t\\) reduction\\(\\+:r\\) if\\(parallel:D.\[0-9\]+\\) num_threads\\(D.\[0-9\]+\\) default\\(shared\\) proc_bind\\(spread\\)" "original" } }

