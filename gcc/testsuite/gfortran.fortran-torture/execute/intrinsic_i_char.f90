! simple test for the CHAR and ICHAR intrinsics
! PR 16579
DO I=0,255
   IF (ICHAR(CHAR(I)) /= I) CALL ABORT()
END DO
END
