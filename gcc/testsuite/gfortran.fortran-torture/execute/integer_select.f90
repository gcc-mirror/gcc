PROGRAM Test_INTEGER_select

! Every wrong branch leads to destruction.

  INTEGER, PARAMETER :: maxI = HUGE (maxI)
  INTEGER, PARAMETER :: minI = -1 * maxI
  INTEGER :: I = 0

  SELECT CASE (I)
     CASE (:-1)
        STOP 1
     CASE (1:)
        STOP 2
     CASE DEFAULT
        CONTINUE
  END SELECT

  SELECT CASE (I)
     CASE (3,2,1)
        STOP 3
     CASE (0)
        CONTINUE
     CASE DEFAULT
        STOP 4
  END SELECT

! Not aborted by here, so it worked
! See about weird corner cases

  I = maxI

  SELECT CASE (I)
     CASE (:-1)
        STOP 5
     CASE (1:)
        CONTINUE
     CASE DEFAULT
        STOP 6
  END SELECT

  SELECT CASE (I)
     CASE (3,2,1,:0)
        STOP 7
     CASE (maxI)
        CONTINUE
     CASE DEFAULT
        STOP 8
  END SELECT

  I = minI

  SELECT CASE (I)
     CASE (:-1)
        CONTINUE
     CASE (1:)
        STOP 9
     CASE DEFAULT
        STOP 10
  END SELECT

  SELECT CASE (I)
     CASE (3:,2,1,0)
        STOP 11
     CASE (minI)
        CONTINUE
     CASE DEFAULT
        STOP 12
  END SELECT

END

