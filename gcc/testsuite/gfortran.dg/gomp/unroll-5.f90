! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll partial
  do i = 1,100
    call dummy(i)
  end do
end subroutine test1

! { dg-final { scan-tree-dump "#pragma omp unroll partial" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp" "gimple" } }
! { dg-final { scan-tree-dump "\.ANNOTATE \\\(\[^\n\r\]*, 1, 8\\\);" "gimple" } }
