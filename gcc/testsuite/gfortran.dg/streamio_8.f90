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
  r = 12.25
  OPEN(UNIT=11, ACCESS="stream")
  inquire(unit=11, pos=mypos)
  if (mypos.ne.1) call abort()
  WRITE(11) "first"
  inquire(unit=11, pos=mypos)
  if (mypos.ne.6) call abort()
  WRITE(11) "second"
  inquire(unit=11, pos=mypos)
  if (mypos.ne.12) call abort()
  WRITE(11) 1234567
  inquire(unit=11, pos=mypos)
  if (mypos.ne.16) call abort()
  write(11) r
  r = 0.0
  inquire (11, pos=mypos)
  read(11,pos=16)r
  if (r.ne.12.25) call abort()
  inquire(unit=11, pos=mypos)
  inquire(unit=11, access=mystring)
  if (mypos.ne.24) call abort()
  if (mystring.ne."STREAM") call abort()
  CLOSE(UNIT=11, status="delete")
END PROGRAM stream_io_8