LOGICAL :: L = .FALSE.

SELECT CASE (L)
   CASE (.TRUE.)
      CALL abort
   CASE (.FALSE.)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT

SELECT CASE (L)
   CASE (.TRUE., .FALSE.)
      CONTINUE
   CASE DEFAULT
      CALL abort                  
END SELECT

SELECT CASE (L)
   CASE (.FALSE.)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT

SELECT CASE (L)
   CASE (.NOT. .TRUE.)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT

SELECT CASE (.NOT. L)
   CASE (.TRUE.)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT

SELECT CASE (Truth_or_Dare() .OR. L)
   CASE (.TRUE.)
      CONTINUE
   CASE DEFAULT
      CALL abort
END SELECT

CONTAINS

   FUNCTION Truth_or_Dare ()
      LOGICAL Truth_or_Dare
      Truth_or_Dare = .TRUE.
   END FUNCTION

END

