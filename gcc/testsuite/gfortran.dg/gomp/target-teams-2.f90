! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

! Check that the expression is evaluated before the target region

subroutine sub(n)
implicit none
integer, value :: n

!$omp target
!$omp teams thread_limit(n+1)
!$omp end teams
!$omp end target
end
! { dg-final { scan-tree-dump "D\.\[0-9\]+ = n \\+ 1;\[\\n\\r\]+ *#pragma omp target thread_limit\\(D\.\[0-9\]+\\) firstprivate\\(D\.\[0-9\]+\\)\[\\n\\r\{ \]+#pragma omp teams thread_limit\\(D\.\[0-9\]+\\)" "original" } }

! { dg-final { scan-tree-dump "D\.\[0-9\]+ = n \\+ 1;\[\\n\\r\]+ *_\[0-9\]+ = D\.\[0-9\]+;\[\\n\\r\]+ *#pragma omp target num_teams\\(0\\) thread_limit\\(_\[0-9\]+\\) firstprivate\\(D\.\[0-9\]+\\)\[\\n\\r\{ \]+integer\\(kind=4\\) D\.\[0-9\]+;\[\\n\\r \]+D\.\[0-9\]+ = D\.\[0-9\]+;\[\\n\\r \]+#pragma omp teams thread_limit\\(D\.\[0-9\]+\\)" "gimple" } }

