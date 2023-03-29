subroutine foo (i, a)
  implicit none
  integer, value :: i
  integer :: a(:)
  integer :: j

  j = 7
  !$omp assume no_openmp, absent (target, teams) holds (i < 32) holds (i < 32_2)
  !$omp end assume

  !$omp assume no_openmp_routines, contains (simd)
  block
    !$omp simd
    do j = 1, i
      a(i) = j
    end do
  end block

  !$omp assume no_parallelism, contains (error)
  if (i >= 32) then
    !$omp error at (execution) message ("Should not happen")
  end if
  !$omp end assume
end
