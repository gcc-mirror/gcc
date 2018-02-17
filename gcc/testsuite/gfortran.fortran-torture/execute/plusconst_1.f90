! PR14005
! The GMP conversion routines object to a leading "+"
program plusconst_1
  implicit none
  real p
  integer i
  data p /+3.1415/
  data i /+42/
  real :: q = +1.234
  integer :: j = +100

  if ((p .ne. 3.1415) .or. (i .ne. 42) .or. (q .ne. 1.234) .or. (j .ne. 100)) &
    STOP 1
end program

