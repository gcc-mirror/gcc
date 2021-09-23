! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

integer function foo(a,b, n) result(r)
  implicit none
  integer :: a(n), b(n), n, i
  r = 0
  !$omp parallel do reduction (inscan, +:r) default(none) firstprivate (a, b)
  do i = 1, n
    r = r + a(i)
    !$omp scan inclusive (r)
    b(i) = r
  end do
end

! { dg-final { scan-tree-dump-times "#pragma omp parallel firstprivate\\(a\\) firstprivate\\(b\\) shared\\(r\\) default\\(none\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for reduction\\(inscan,\\\+:r\\) nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp scan inclusive\\(r\\)" 1 "original" } }
