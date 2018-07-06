! { dg-do run }
! { dg-options "-fno-range-check" }
! PR 52428 Read IO of integers near the end of range. Note that we
! support the two's complement representation even though the Fortran
! numerical model has a symmetric range.  (The -fno-range-check option
! is needed to allow the -2147483648 literal.)
program int_range
  implicit none
  character(25) :: inputline = "-2147483648"
  integer(4) ::  test
  integer :: st

  read(inputline,100) test
100 format(1i11)
  if (test /= -2147483648) STOP 1
  inputline(1:1) = " "
  read(inputline, 100, iostat=st) test
  if (st == 0) STOP 2
  inputline(11:11) = "7"
  read(inputline, 100) test
  if (test /= 2147483647) STOP 3

  ! Same as above but with list-formatted IO
  inputline = "-2147483648"
  read(inputline, *) test
  if (test /= -2147483648) STOP 4
  inputline(1:1) = " "
  read(inputline, *, iostat=st) test
  if (st == 0) STOP 5
  inputline(11:11) = "7"
  read(inputline, *) test
  if (test /= 2147483647) STOP 6

end program int_range
