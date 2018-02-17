INTEGER         :: I = 1
SELECT CASE (I)
   CASE (-3:-5)  ! Can never be matched
      STOP 1
   CASE (1)
      CONTINUE
   CASE DEFAULT
      STOP 2
END SELECT

I = -3
SELECT CASE (I)
   CASE (-3:-5) ! Can never be matched
      STOP 3
   CASE (1)
      CONTINUE
   CASE DEFAULT
      CONTINUE
END SELECT

I = -5
SELECT CASE (I)
   CASE (-3:-5) ! Can never be matched
      STOP 4
   CASE (-5)
      CONTINUE
   CASE DEFAULT
      STOP 5
END SELECT
END

