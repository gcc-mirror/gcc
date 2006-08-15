! { dg-do run }
! PR25828 Stream IO test 1
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
PROGRAM stream_io_1
  IMPLICIT NONE
  integer(kind=4) i
  real(kind=8) r
  OPEN(UNIT=11, ACCESS="stream")
  WRITE(11) "first"
  WRITE(11) "second"
  WRITE(11) 1234567
  write(11) 3.14159_8
  read(11, pos=12)i
  if (i.ne.1234567) call abort()
  read(11) r
  if (r-3.14159 .gt. 0.00001) call abort()
  CLOSE(UNIT=11, status="delete")
END PROGRAM stream_io_1