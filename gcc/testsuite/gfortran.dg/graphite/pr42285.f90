! { dg-options "-O2 -floop-nest-optimize" }

SUBROUTINE EFGRDM(NCF,NFRG,G,RTRMS,GM,IOPT,K1)
  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
  DIMENSION G(*),RTRMS(*),GM(*)

  DUM = 0
  DO I=1,NFRG
     DO J=1,3
        IF (IOPT.EQ.0) THEN
           GM(K1)=G(K1)
        END IF
     END DO
     DO J=1,3
        JDX=NCF*9+IOPT*9*NFRG
        DO M=1,3
           DUM=DUM+RTRMS(JDX+M)
        END DO
        GM(K1)=DUM
     END DO
  END DO
  RETURN
END SUBROUTINE EFGRDM

