! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-gimple" }

program test
  implicit none
  integer, parameter :: N = 100
  real :: a(N)
  
  !$omp target map(from: a)
    call f (a, 3.14159)
  !$omp end target

  call f (a, 2.71828)
contains
  subroutine f (a, x)
    integer :: i
    real :: a(N), x
    !$omp declare target

    !$omp metadirective &
    !$omp&  when (construct={target}: distribute parallel do ) &
    !$omp&  default(parallel do simd)
      do i = 1, N
 	a(i) = x * i
      end do
  end subroutine
end program

! The metadirective should be resolved during Gimplification.

! { dg-final { scan-tree-dump-times "#pragma omp metadirective" 1 "original" } }
! { dg-final { scan-tree-dump-times "when \\(construct = .*target.*\\):" 1 "original" } }
! { dg-final { scan-tree-dump-times "otherwise:" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 2 "original" } }

! { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" } }
