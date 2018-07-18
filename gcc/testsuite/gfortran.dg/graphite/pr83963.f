! { dg-do compile }
! { dg-options "-O -floop-nest-optimize" }

      SUBROUTINE DAVCI(NORB,NCOR,NCI,NA,NB,
     *    CI,MAXP,MAXW1,
     *      IHMCON,ISTRB,ISTRP,ISTAR,II)
      DIMENSION EC(MAXP,MAXP),IWRK1(2*MAXW1)
         EC(II,II) = 1.0D+00
         DO 1396 II=1,MAXP
            DO 1398 JJ=1,II-1
               EC(II,JJ) = 0.0D+00
 1398       CONTINUE
 1396    CONTINUE
      IF (NA.EQ.NB) THEN
      CALL RINAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI(1,IP),IACON1,IBCON1,
     *       IWRK1,IHMCON,ISTRB,ISTRP,ISTAR)
      ENDIF
      END
