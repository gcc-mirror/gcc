! { dg-do run }
! PR33985 Stream IO test with empty write, array writes, and reads.
program streamtest
  implicit none
  character(1)   :: lf = char(10)
  character(1)   :: tchar
  integer        :: i,j,k
  real(kind=4), dimension(100,100) :: anarray
  open(10, file="teststream", access="stream", form="unformatted")
  anarray = 3.14159
  write(10) anarray
  write(10, pos=1) ! This is a way to position an unformatted file
  anarray = 0.0
  read(10) anarray
  anarray = abs(anarray - 3.14159)
  if (any(anarray.gt.0.00001)) call abort()
  close(10,status="delete")
end program streamtest