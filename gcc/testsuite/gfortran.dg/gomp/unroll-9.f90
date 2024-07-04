! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll full
  !$omp unroll partial(3)
  !$omp unroll partial(2)
  !$omp unroll partial(1)
  do i = 1,100
    call dummy(i)
  end do
end subroutine test1

! { dg-final { scan-tree-dump "#pragma omp unroll full" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(1\\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(2\\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp unroll partial\\\(3\\\)" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp unroll" "gimple" } }
! { dg-final { scan-tree-dump "\.ANNOTATE \\\(\[^\n\r]*, 1, 2\\\);" "gimple" } }
! { dg-final { scan-tree-dump "\.ANNOTATE \\\(\[^\n\r]*, 1, 3\\\);" "gimple" } }
! { dg-final { scan-tree-dump "\.ANNOTATE \\\(\[^\n\r]*, 1, 17\\\);" "gimple" } }
