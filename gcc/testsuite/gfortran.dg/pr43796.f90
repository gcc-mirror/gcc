! { dg-do compile }
! { dg-options "-O2 -fcheck=bounds" }

    FUNCTION F06FKFN(N,W,INCW,X,INCX)
       IMPLICIT NONE
       INTEGER, PARAMETER :: WP = KIND(0.0D0)
       REAL (KIND=WP)                  :: F06FKFN
       REAL (KIND=WP), PARAMETER       :: ONE = 1.0E+0_WP
       REAL (KIND=WP), PARAMETER       :: ZERO = 0.0E+0_WP
       INTEGER, INTENT (IN)            :: INCW, INCX, N
       REAL (KIND=WP), INTENT (IN)     :: W(*), X(*)
       REAL (KIND=WP)                  :: ABSYI, NORM, SCALE, SSQ
       INTEGER                         :: I, IW, IX
       REAL (KIND=WP), EXTERNAL        :: F06BMFN
       INTRINSIC                          ABS, SQRT
       IF (N<1) THEN
          NORM = ZERO
       ELSE IF (N==1) THEN
          NORM = SQRT(W(1))*ABS(X(1))
       ELSE
          IF (INCW>0) THEN
             IW = 1
          ELSE
             IW = 1 - (N-1)*INCW
          END IF
          IF (INCX>0) THEN
             IX = 1
          ELSE
             IX = 1 - (N-1)*INCX
          END IF
          SCALE = ZERO
          SSQ = ONE
          DO I = 1, N
             IF ((W(IW)/=ZERO) .AND. (X(IX)/=ZERO)) THEN
                ABSYI = SQRT(W(IW))*ABS(X(IX))
                IF (SCALE<ABSYI) THEN
                   SSQ = 1 + SSQ*(SCALE/ABSYI)**2
                   SCALE = ABSYI
                ELSE
                   SSQ = SSQ + (ABSYI/SCALE)**2
                END IF
             END IF
             IW = IW + INCW
             IX = IX + INCX
          END DO
          NORM = F06BMFN(SCALE,SSQ)
       END IF
       F06FKFN = NORM
       RETURN
    END FUNCTION F06FKFN

