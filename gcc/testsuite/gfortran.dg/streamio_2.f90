! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR25828 Stream IO test 2
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
PROGRAM readUstream
  IMPLICIT NONE
  CHARACTER*3 :: string
  INTEGER :: n
  string = "123"
  n = 13579
  OPEN(UNIT=11, FILE="streamio2", ACCESS="STREAM")
  WRITE(11) "first"
  WRITE(11) "second"
  WRITE(11) 7
  READ(11, POS=3) string
  READ(11, POS=12) n
  if (string.ne."rst") STOP 1
  if (n.ne.7) STOP 2
  close(unit=11, status="delete")
END PROGRAM readUstream

