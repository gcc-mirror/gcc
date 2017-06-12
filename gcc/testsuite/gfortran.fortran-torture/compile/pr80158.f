      SUBROUTINE DRPAUL(SMAT,TMAT,EPS,EPT,SIJ,TIJ,WRK,VEC,ARRAY,FMO,
     *                  XMKVIR,TMJ,XMI,YMI,ZMI,ZQQ,L1,L1EF,LNA,LNA2,
     *                  NAEF,L2,NLOC,NVIR,PROVEC,FOCKMA,MXBF,MXMO2)
      DIMENSION CMO(L1,L1),TLOC(LNA,LNA),SMJ(L1,NAEF),XMK(L1,LNA)
      DO I = 1,LNA
         DO J = 1,LNA
            IF (I.LE.NOUT) TLOC(I,J) = ZERO
            IF (J.LE.NOUT) TLOC(I,J) = ZERO
         END DO
         DO NA=1,NOC
            IF ( ABS(E(NI)-E(NA)) .GE.TOL) THEN
            END IF
         END DO
      END DO
      END
      
