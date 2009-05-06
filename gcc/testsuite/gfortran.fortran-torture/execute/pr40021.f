C Derived from lapack
        PROGRAM test
        DOUBLE PRECISION DA
        INTEGER I, N
        DOUBLE PRECISION DX(9),DY(9)

        EXTERNAL DAXPY
        N=5
        DA=1.0
        DATA DX/-2, -1, -3, -4, 1, 2, 10, 15, 14/
        DATA DY/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
        CALL DAXPY (N,DA,DX,DY)
        DO 10 I = 1, N
          if (DX(I).ne.DY(I)) call abort
10      CONTINUE
        STOP
        END

      SUBROUTINE DAXPY(N,DA,DX,DY)
      DOUBLE PRECISION DA
      INTEGER N
      DOUBLE PRECISION DX(*),DY(*)
      INTEGER I,IX,IY,M,MP1
      INTRINSIC MOD
      IF (N.LE.0) RETURN
   20 M = MOD(N,4)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF (N.LT.4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
          DY(I) = DY(I) + DA*DX(I)
          DY(I+1) = DY(I+1) + DA*DX(I+1)
          DY(I+2) = DY(I+2) + DA*DX(I+2)
          DY(I+3) = DY(I+3) + DA*DX(I+3)
   50 CONTINUE
      RETURN
      END
