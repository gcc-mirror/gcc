! { dg-do compile }
! { dg-options "-O3 -fsplit-loops" }
      SUBROUTINE SSYMM(C,LDC)
      REAL C(LDC,*)
      LOGICAL LSAME
      LOGICAL UPPER
      IF (LSAME) THEN
          DO 170 J = 1,N
              DO 140 K = 1,J  
                  IF (UPPER) THEN
                      END IF
  140         CONTINUE
              DO 160 K = J + 1,N
                  C(I,J) = B(K)
  160         CONTINUE
  170     CONTINUE
      END IF
      END
