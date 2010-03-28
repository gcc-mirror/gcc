 MODULE MAIN1
      INTEGER , PARAMETER :: MXGLVL = 87
      CHARACTER(8) :: SRCTYP
      REAL :: GRIDWS(MXGLVL)
      REAL :: ZI, HS
    END MODULE MAIN1

    PROGRAM TEST
      USE MAIN1
      IF (HS >= ZI) THEN
      ELSEIF (      SRCTYP == 'AREA'     &
               .OR. SRCTYP == 'AREAPOLY' &
               .OR. SRCTYP == 'AREACIRC' &
               .OR. SRCTYP == 'OPENPIT'   ) THEN
         CALL ANYAVG (MXGLVL, GRIDWS)
         CALL ANYAVG (MXGLVL, GRIDWS)
      ELSE
         IF ( HS > 0.0 ) THEN
            CALL ANYAVG (MXGLVL, GRIDWS)
            CALL ANYAVG (MXGLVL, GRIDWS)
            CALL ANYAVG (MXGLVL, GRIDWS)
         ENDIF
      ENDIF
      IF (HS.LT.ZI) THEN
         ZI = HS
      ENDIF
    contains
      SUBROUTINE ANYAVG(NLVLS,HTS)
        INTEGER NLVLS
        REAL HTS(NLVLS)
        IF (5.LT.NLVLS) THEN
           CALL GINTRP (HTS(5),HTS(5+1))
        ENDIF
        CALL GINTRP (HTS(5-1), HTS(5))
      END SUBROUTINE ANYAVG

      subroutine gintrp (x1, x2)
        print *, x1, x2
      end subroutine

    END PROGRAM TEST


