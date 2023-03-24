! { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
! { dg-additional-options "-fdump-tree-original" }

subroutine test1
  implicit none
  integer :: i
  !$omp parallel do collapse(1)
  !$omp unroll partial(4) ! { dg-optimized {replaced consecutive 'omp unroll' directives by 'omp unroll auto\(24\)'} }
  !$omp unroll partial(3)
  !$omp unroll partial(2)
  !$omp unroll partial(1)
  do i = 1,100
     call dummy(i)
  end do
end subroutine test1

! Loop should be unrolled 1 * 2 * 3 * 4 = 24 times

! { dg-final { scan-tree-dump {#pragma omp for nowait collapse\(1\) unroll_partial\(4\).0 unroll_partial\(3\).0 unroll_partial\(2\).0 unroll_partial\(1\)} "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp loop_transform" "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times "dummy" 24 "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {#pragma omp for} 1 "omp_transform_loops" } }
