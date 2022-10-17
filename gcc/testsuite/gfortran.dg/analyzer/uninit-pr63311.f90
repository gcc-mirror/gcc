! { dg-additional-options "-O0" }

MODULE M1
  IMPLICIT NONE
CONTAINS
  INTEGER FUNCTION foo()
     INTEGER, VOLATILE :: v=42
     foo=v
  END FUNCTION
  SUBROUTINE test(n,flag)
    INTEGER :: n,i,j,k,l,tt
    LOGICAL :: flag
    REAL(KIND=8) :: v,t
    IF (flag) THEN
      t=42
      tt=foo()
    ENDIF
    v=0
    DO i=1,n
       v=0
       IF (flag) THEN
          IF (tt==i) v=MAX(v,t)
       ENDIF
       DO j=1,n
        DO k=1,n
            v=MAX(v,sin(REAL(j*k)))
         ENDDO
       ENDDO
    ENDDO
  END SUBROUTINE
END MODULE M1

USE M1
INTEGER :: n
LOGICAL :: flag
n=4
flag=.FALSE.
CALL test(n,flag)
END
