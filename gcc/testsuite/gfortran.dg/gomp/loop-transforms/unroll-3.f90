! { dg-additional-options "-fdump-tree-omp_transform_loops" }
! { dg-additional-options "-fdump-tree-original" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll full
  do i = 1,10
     call dummy(i)
  end do
end subroutine test1

! Loop should be removed with 10 copies of the body remaining

! { dg-final { scan-tree-dump-times "dummy" 10 "omp_transform_loops" } }
! { dg-final { scan-tree-dump "#pragma omp loop_transform" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } }
