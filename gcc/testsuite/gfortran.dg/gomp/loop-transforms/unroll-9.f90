! { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
! { dg-additional-options "-fdump-tree-original" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll full ! { dg-optimized {removed useless 'omp unroll auto' directives preceding 'omp unroll full'} }
  !$omp unroll partial(3)
  !$omp unroll partial(2)
  !$omp unroll partial(1)
  do i = 1,100
     call dummy(i)
  end do
end subroutine test1

! { dg-final { scan-tree-dump {#pragma omp loop_transform unroll_full.0 unroll_partial\(3\).0 unroll_partial\(2\).0 unroll_partial\(1\).0} "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp unroll" "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times "dummy" 100 "omp_transform_loops" } }
