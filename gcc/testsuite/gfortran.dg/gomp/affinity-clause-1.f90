! { dg-additional-options "-fdump-tree-original" }
subroutine foo(x)
  integer :: x
  integer :: a, b(5), cc, d(5,5)
  !$omp taskgroup
    !$omp task affinity(a)
    !$omp end task
    !$omp task affinity(iterator(i=int(cos(1.0+a)):5, jj =2:5:2) : b(i), d(i,jj))
    !$omp end task
    !$omp task affinity(iterator(i=int(cos(1.0+a)):5) : b(i), d(i,i))
    !$omp end task
    !$omp task affinity (iterator(i=1:5): a)
    !$omp end task
    !$omp task affinity (iterator(i=1:5): a) affinity(iterator(i=1:5) : x)
    !$omp end task
    !$omp task affinity (iterator(integer(8) :: j=1:5, k=7:4:-1) : b(j+k),a) affinity (cc)
    !$omp end task
 !$omp end taskgroup
end

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(a\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = .integer.kind=4.. __builtin_cosf ..real.kind=4.. a \\+ 1.0e\\+0\\);" 2 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(integer\\(kind=4\\) jj=2:5:2, integer\\(kind=4\\) i=D\\.\[0-9\]+:5:1\\):b\\\[.* <?i>? \\+ -1\\\]\\) affinity\\(iterator\\(integer\\(kind=4\\) jj=2:5:2, integer\\(kind=4\\) i=D\\.\[0-9\]+:5:1\\):d\\\[\\(.*jj \\* 5 \\+ .* <?i>?\\) \\+ -6\\\]\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(integer\\(kind=4\\) i=D\\.\[0-9\]+:5:1\\):b\\\[.* <?i>? \\+ -1\\\]\\) affinity\\(iterator\\(integer\\(kind=4\\) i=D\\.\[0-9\]+:5:1\\):d\\\[\\(.*i \\+ -1\\) \\* 6\\\]\\)"  1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(integer\\(kind=4\\) i=1:5:1\\):a\\)\[^ \]" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(integer\\(kind=4\\) i=1:5:1\\):a\\) affinity\\(iterator\\(integer\\(kind=4\\) i=1:5:1\\):\\*x\\)"  1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(integer\\(kind=4\\) k=7:4:-1, integer\\(kind=8\\) j=1:5:1\\):b\\\[\\(?\\(integer\\(kind=.\\).* \[jk\] \\+ .*\[kj\]\\) \\+ -1\\\]\\) affinity\\(iterator\\(integer\\(kind=4\\) k=7:4:-1, integer\\(kind=8\\) j=1:5:1\\):a\\) affinity\\(cc\\)" 1 "original" } }
