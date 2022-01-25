! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-gimple" }
! { dg-additional-options "-fdump-tree-optimized" }

module test
  integer, parameter :: N = 100
contains
  subroutine f (x, y, z)
    integer :: x(N), y(N), z(N)

    !$omp target map (to: v1, v2) map(from: v3)
      !$omp metadirective &
		!$omp& when(device={arch("nvptx")}: teams loop) &
		!$omp& default(parallel loop)
	do i = 1, N
	  z(i) = x(i) * y(i)
	enddo
    !$omp end target
  end subroutine
end module

! The metadirective should be resolved after Gimplification.

! { dg-final { scan-tree-dump-times "#pragma omp metadirective" 1 "original" } }
! { dg-final { scan-tree-dump-times "when \\(device arch .nvptx.\\):" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams" 1 "original" } }
! { dg-final { scan-tree-dump-times "default:" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp loop" 2 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp metadirective" 1 "gimple" } }

! { dg-final { scan-tree-dump-not "#pragma omp metadirective" "optimized" } }
