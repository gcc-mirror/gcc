! { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
! { dg-additional-options "-fdump-tree-original" }

subroutine test1
  implicit none
  integer :: i
  !$omp unroll partial ! { dg-optimized {'partial' clause without unrolling factor turned into 'partial\(5\)' clause} }
  do i = 1,100
     call dummy(i)
  end do
end subroutine test1

! Loop should be unrolled 5 times and the internal representation should be lowered.

! { dg-final { scan-tree-dump {#pragma omp loop_transform unroll_partial} "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times "dummy" 5 "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {if \(i\.[0-9]+ < .+?.+goto.+else goto.*?$} 1 "omp_transform_loops" } }
