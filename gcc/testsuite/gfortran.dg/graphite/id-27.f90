! { dg-do compile }
! { dg-additional-options "-Ofast -std=legacy" }
MODULE module_ra_gfdleta
      INTEGER, PARAMETER              :: NBLY=15
      REAL   , SAVE :: EM1(28,180),EM1WDE(28,180),TABLE1(28,180),     &
                           TABLE2(28,180),TABLE3(28,180),EM3(28,180), &
                           SOURCE(28,NBLY), DSRCE(28,NBLY)
CONTAINS
      SUBROUTINE TABLE 
 INTEGER, PARAMETER :: NBLX=47
 INTEGER , PARAMETER:: NBLW = 163
      REAL ::  &
               SUM(28,180),PERTSM(28,180),SUM3(28,180),       &
               SUMWDE(28,180),SRCWD(28,NBLX),SRC1NB(28,NBLW), &
               DBDTNB(28,NBLW)
      REAL ::  &
               ZMASS(181),ZROOT(181),SC(28),DSC(28),XTEMV(28), &
               TFOUR(28),FORTCU(28),X(28),X1(28),X2(180),SRCS(28), &
               R1T(28),R2(28),S2(28),T3(28),R1WD(28)
      REAL ::  EXPO(180),FAC(180)
      I = 0
      DO 417 J=121,180
      FAC(J)=ZMASS(J)*(ONE-(ONE+X2(J))*EXPO(J))/(X2(J)*X2(J))
417   CONTINUE
      DO 421 J=121,180
      SUM3(I,J)=SUM3(I,J)+DBDTNB(I,N)*FAC(J)
421   CONTINUE
      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 420 J=1,180
         DO 420 I=1,28
         SUMWDE(I,J)=SUMWDE(I,J)+SRC1NB(I,N)*EXPO(J)
420      CONTINUE
      ENDIF
      DO 433 J=121,180
      EM3(I,J)=SUM3(I,J)/FORTCU(I)
433   CONTINUE
      DO 501 I=1,28
      EM1WDE(I,J)=SUMWDE(I,J)/TFOUR(I)
501   CONTINUE
      END SUBROUTINE TABLE
      END MODULE module_RA_GFDLETA
