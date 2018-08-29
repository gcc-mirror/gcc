! { dg-do run { target fd_truncate } }
! Wide character I/O test 3, unformatted arrays
! Test case developed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program test1
  integer, parameter :: k4 = 4
  character(len=10,kind=4) :: wide
  character(len=10,kind=4), dimension(5,7) :: widearray
  wide = k4_"abcdefg"
  widearray = k4_"1234abcd"
  open(10, form="unformatted", status="scratch")
  write(10) wide
  rewind(10)
  wide = "wrong"
  read(10) wide
  if (wide /= k4_"abcdefg") STOP 1
  rewind(10)
  write(10) widearray(2:4,3:7)
  widearray(2:4,3:7)=""
  rewind(10)
  read(10) widearray(2:4,3:7)
  close(10)
  if (any(widearray.ne.k4_"1234abcd")) STOP 2
end program test1
