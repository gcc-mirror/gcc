! PR fortran/95869
! { dg-do compile }

program pr95869
  implicit none

  integer, parameter :: N = 100
  integer, parameter :: LIMIT = 60
  integer :: i, j
  integer, dimension(N) :: a = (/ (i, i = 1,N) /)
  do j = 1, N
    !$omp target parallel if(j .lt. LIMIT) map(tofrom: a(1:N))
    do i = 1, N
      a(i) = a(i) + 1
    end do
    !$omp end target parallel
    end do
end program
