MODULE TEST
CONTAINS
PURE FUNCTION s2a_3(s1,s2,s3) RESULT(a)
    CHARACTER(LEN=*), INTENT(IN)             :: s1, s2, s3
    CHARACTER(LEN=4), DIMENSION(3)        :: a

  a(1)=s1; a(2)=s2; a(3)=s3
END FUNCTION
END MODULE

USE TEST
character(len=12) :: line
write(line,'(3A4)') s2a_3("a","bb","ccc")
IF (line.NE."a   bb  ccc") CALL ABORT()
END

