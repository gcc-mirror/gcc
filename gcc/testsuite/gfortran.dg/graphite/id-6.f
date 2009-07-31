      SUBROUTINE EIJDEN(EPS,V,E,IA,WRK,L1,L2,L3,L0,ECI)
      DIMENSION V(L1,L0),EPS(L2),E(*),IA(L1),WRK(L1),ECI(L0,L0)
      IF(SCFTYP.EQ.RHF .AND. MPLEVL.EQ.0 .AND.
     *   CITYP.NE.GUGA .AND. CITYP.NE.CIS) THEN
            CALL DCOPY(NORB,E(IADDE),1,E(IADD),1)
      END IF
      IF (CITYP.NE.GUGA) THEN
      DO 500 I = 1,L1
         DO 430 L = 1,NORB
            DO 420 K = 1,NORB
               IF(K.LE.L) THEN
                  WRK(L) = WRK(L) - V(I,K)*ECI(K,L)
               ELSE
                  WRK(L) = WRK(L) - V(I,K)*ECI(L,K)
               END IF
  420       CONTINUE
  430    CONTINUE
         DO 440 L = 1,NORB
  440    CONTINUE
  500 CONTINUE
      END IF
      END
