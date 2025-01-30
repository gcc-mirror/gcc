! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

module test
  integer, parameter :: N = 100
contains
  subroutine f (a, flag)
    integer :: a(N)
    logical :: flag
    integer :: i
    
   !$omp metadirective &
   !$omp&  when (user={condition(flag)}: &
   !$omp&	 target teams distribute parallel do map(from: a(1:N))) &
   !$omp&  default(parallel do)
     do i = 1, N
       a(i) = i
     end do
  end subroutine
end module

! The metadirective should be resolved at parse time, but is currently
! resolved during Gimplification

! { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp target" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams" 1 "gimple" } }
! { dg-final { scan-tree-dump-times  "#pragma omp distribute" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp for" 2 "gimple" } }
