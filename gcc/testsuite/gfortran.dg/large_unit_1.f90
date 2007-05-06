! { dg-do run }
! { dg-shouldfail "Unit number in I/O statement too large" }
! PR31201  Unit number in I/O statement too large
! Test case from PR
      integer(kind=8)  :: k= 2_8**36 + 10
      integer(kind=4)  :: j= 10
      logical  ex,op
      INQUIRE(unit=k, exist=ex,opened=op)
      print *, ex, op
      IF (ex) THEN
         OPEN(unit=k)
         INQUIRE(unit=j, opened=op)
         IF (op) CALL ABORT()
      ENDIF
      print *, k
      close(k)
      end
