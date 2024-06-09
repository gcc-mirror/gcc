! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" } */

subroutine test1
  implicit none
  integer :: i
  !$omp parallel do collapse(1)
  !$omp unroll partial(4)
  !$omp unroll partial(3)
  !$omp unroll partial(2)
  !$omp unroll partial(1)
  do i = 1,100
    call dummy(i)
  end do
end subroutine test1

! Loop should be unrolled 1 * 2 * 3 * 4 = 24 times
! { dg-final { scan-tree-dump "#pragma omp for nowait collapse\\\(1\\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(1\\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(2\\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(3\\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(4\\\)" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp unroll" "gimple" } }
! { dg-final { scan-tree-dump-times "\.ANNOTATE \\\(\[^\n\r\]*, 1, 2\\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "\.ANNOTATE \\\(\[^\n\r\]*, 1, 3\\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "\.ANNOTATE \\\(\[^\n\r\]*, 1, 4\\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp for" 1 "gimple" } }
