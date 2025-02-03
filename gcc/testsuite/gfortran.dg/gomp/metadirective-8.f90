! { dg-do compile }

program test
  integer :: i
  integer, parameter :: N = 100
  integer :: sum = 0
  
  ! The compiler should never consider a situation where both metadirectives
  ! match, but that does not matter because the spec says "Replacement of
  ! the metadirective with the directive variant associated with any of the
  ! dynamic replacement candidates must result in a conforming OpenMP
  ! program.  So the second metadirective is rejected as not being
  ! a valid loop-nest even if the first one does not match.
  
!$omp metadirective when (implementation={vendor("ibm")}: &
  !$omp&  target teams distribute)
    !$omp metadirective when (implementation={vendor("gnu")}: parallel do) ! { dg-error "Unexpected !.OMP METADIRECTIVE statement" }
      do i = 1, N
	sum = sum + i
      end do
end program

