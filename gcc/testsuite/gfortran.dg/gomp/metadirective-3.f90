! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

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

! If offload device "nvptx" isn't supported, the front end can eliminate
!  that alternative and not produce a metadirective at all.  Otherwise this
!  won't be resolved until late.
! { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" { target { ! offload_nvptx } } } }
! { dg-final { scan-tree-dump "#pragma omp metadirective" "gimple" { target { offload_nvptx } } } }
