! { dg-additional-options "-fdump-tree-original" }
implicit none
integer :: iterator(10), i

!$omp taskgroup
  !$omp task affinity(iterator)
  !$omp end task

  !$omp task affinity(iterator(3))
  !$omp end task

  !$omp task affinity(iterator(i=1:10) : iterator(i))
  !$omp end task

!$omp end taskgroup

end

! { dg-final { scan-tree-dump-times "pragma omp task affinity\\(iterator\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\\[2\\\]\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(integer\\(kind=4\\) i=1:10:1\\):iterator\\\[.* <?i>? \\+ -1\\\]\\)" 1 "original" } }
