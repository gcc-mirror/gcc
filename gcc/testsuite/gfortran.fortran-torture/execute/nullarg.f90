! This is the testcase from PR 12841. We used to report a type/rank mismatch
! when passing NULL() as an argument to a function.
   MODULE T
   PUBLIC :: A
   CONTAINS
   SUBROUTINE A(B)
   REAL, POINTER :: B
   IF (ASSOCIATED(B)) CALL ABORT()
   END SUBROUTINE A
   END MODULE T
   USE T
   CALL A(NULL())
   END
