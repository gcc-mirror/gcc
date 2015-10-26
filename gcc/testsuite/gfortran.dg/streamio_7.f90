! { dg-do run }
! PR25828 Stream IO test 7, Array writes and reads.
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program streamtest
  implicit none
  character(1)   :: lf = char(10)
  character(1)   :: tchar
  integer        :: i,j,k
  real(kind=4), dimension(100,100) :: anarray
  open(10, file="teststream_streamio_7", access="stream", form="unformatted")
  anarray = 3.14159
  write(10) anarray
  anarray = 0.0
  read(10, pos=1) anarray
  anarray = abs(anarray - 3.14159)
  if (any(anarray.gt.0.00001)) call abort()
  close(10,status="delete")
end program streamtest