PROGRAM Test_INTEGER_select

! Every wrong branch leads to destruction.

  INTEGER, PARAMETER :: maxI = HUGE (maxI)
  INTEGER, PARAMETER :: minI = -1 * maxI
  INTEGER :: I = 0

  SELECT CASE (I)
     CASE (:-1)
        CALL abort
     CASE (1:)
        CALL abort
     CASE DEFAULT
        CONTINUE
  END SELECT

  SELECT CASE (I)
     CASE (3,2,1)
        CALL abort
     CASE (0)
        CONTINUE
     CASE DEFAULT
        call abort
  END SELECT

! Not aborted by here, so it worked
! See about weird corner cases

  I = maxI

  SELECT CASE (I)
     CASE (:-1)
        CALL abort
     CASE (1:)
        CONTINUE
     CASE DEFAULT
        CALL abort
  END SELECT

  SELECT CASE (I)
     CASE (3,2,1,:0)
        CALL abort
     CASE (maxI)
        CONTINUE
     CASE DEFAULT
        call abort
  END SELECT

  I = minI

  SELECT CASE (I)
     CASE (:-1)
        CONTINUE
     CASE (1:)
        CALL abort
     CASE DEFAULT
        CALL abort
  END SELECT

  SELECT CASE (I)
     CASE (3:,2,1,0)
        CALL abort
     CASE (minI)
        CONTINUE
     CASE DEFAULT
        call abort
  END SELECT

END

