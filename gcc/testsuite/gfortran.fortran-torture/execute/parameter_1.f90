! Program to test array parameter variables.
program parameter_1
  implicit none
  integer i
  INTEGER, PARAMETER :: ii(10) = (/ (I,I=1,10) /)
  REAL, PARAMETER    :: rr(10) = ii

  do i = 1, 10
    if (ii(i) /= i) STOP 1
    if (rr(i) /= i) STOP 2
  end do
end program parameter_1
