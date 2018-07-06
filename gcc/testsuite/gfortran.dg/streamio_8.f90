! { dg-do run }
! PR25828 Stream IO test 8
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
PROGRAM stream_io_8
  IMPLICIT NONE
  integer(kind=8) mypos
  character(10) mystring
  real(kind=8) r
  mypos = 0
  mystring = "not yet"
  r = 12.25d0
  OPEN(UNIT=11, ACCESS="stream")
  inquire(unit=11, pos=mypos)
  if (mypos.ne.1) STOP 1
  WRITE(11) "first"
  inquire(unit=11, pos=mypos)
  if (mypos.ne.6) STOP 2
  WRITE(11) "second"
  inquire(unit=11, pos=mypos)
  if (mypos.ne.12) STOP 3
  WRITE(11) 1234567_4
  inquire(unit=11, pos=mypos)
  if (mypos.ne.16) STOP 4
  write(11) r
  r = 0.0
  inquire (11, pos=mypos)
  read(11,pos=16)r
  if (abs(r-12.25d0)>1e-10) STOP 5
  inquire(unit=11, pos=mypos)
  inquire(unit=11, access=mystring)
  if (mypos.ne.24) STOP 6
  if (mystring.ne."STREAM") STOP 7
  CLOSE(UNIT=11, status="delete")
END PROGRAM stream_io_8
