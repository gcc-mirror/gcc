INTEGER         :: I = 1
SELECT CASE (I)
   CASE (-3:-5)  ! Can never be matched
      CALL abort
   CASE (1)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT

I = -3
SELECT CASE (I)
   CASE (-3:-5) ! Can never be matched
      CALL abort
   CASE (1)
      CONTINUE
   CASE DEFAULT
      CONTINUE
END SELECT

I = -5
SELECT CASE (I)
   CASE (-3:-5) ! Can never be matched
      CALL abort
   CASE (-5)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT
END

