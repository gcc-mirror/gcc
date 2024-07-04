! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll full
  do i = 1,10
    call dummy(i)
  end do
end subroutine test1

! Loop should be removed with 10 copies of the body remaining
! { dg-final { scan-tree-dump "#pragma omp unroll full" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp" "gimple" } }
! { dg-final { scan-tree-dump "\.ANNOTATE \\\(\[^\n\r\]*, 1, 10\\\);" "gimple" } }
