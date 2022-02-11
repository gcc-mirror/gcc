! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

program test
  integer :: i
  integer, parameter :: N = 100
  integer :: sum = 0
  
  ! The compiler should never consider a situation where both metadirectives
  ! match.  If it does, then the nested metadirective would be an error
  ! as it is not a loop-nest as per the OpenMP specification.

  !$omp metadirective when (implementation={vendor("ibm")}: &
  !$omp&  target teams distribute)
    !$omp metadirective when (implementation={vendor("gnu")}: parallel do)
      do i = 1, N
	sum = sum + i
      end do
end program

! { dg-final { scan-tree-dump-not "when \\(implementation vendor \"ibm\"\\):" "original" } }
! { dg-final { scan-tree-dump-times "when \\(implementation vendor \"gnu\"\\):" 1 "original" } }
