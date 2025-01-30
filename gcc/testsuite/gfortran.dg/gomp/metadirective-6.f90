! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

module test
  integer, parameter :: N = 100
contains
  subroutine f (a, run_parallel, run_guided)
    integer :: a(N)
    logical :: run_parallel, run_guided
    integer :: i

    !$omp begin metadirective when(user={condition(run_parallel)}: parallel)
      !$omp metadirective &
      !$omp&  when(construct={parallel}, user={condition(run_guided)}: &
      !$omp&       do schedule(guided)) &
      !$omp&  when(construct={parallel}: do schedule(static))
	do i = 1, N
	  a(i) = i
	end do
    !$omp end metadirective
  end subroutine
end module

! { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp for" 2 "gimple" } }
