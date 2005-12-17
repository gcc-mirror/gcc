c { dg-do compile }
* =watson11.f in Burley's g77 test suite.
* Probably originally submitted by Ian Watson.
* Too small to worry about copyright issues, IMO, since it
* doesn't do anything substantive.
      SUBROUTINE OUTDNS(A,B,LCONV)
      IMPLICIT REAL(kind=8) (A-H,O-Z),INTEGER(I-N)
      COMMON/ARRAYS/Z(64,8),AB(30,30),PAIRS(9,9),T(9,9),TEMP(9,9),C1(3),
     >  C2(3),AA(30),BB(30)
      EQUIVALENCE (X1,C1(1)),(Y1,C1(2)),(Z1,C1(3))
      EQUIVALENCE (X2,C2(1)),(Y2,C2(2)),(Z2,C2(3))
      COMMON /CONTRL/
     >  SHIFT,CONV,SCION,DIVERG,
     >  IOPT,KCNDO,KINDO,KMINDO,I2EINT,KOHNO,KSLATE,
     >  N,NG,NUMAT,NSEK,NELECS,NIT,OCCA,OCCB,NOLDAT,NOLDFN
      INTEGER OCCA,OCCB
      DIMENSION W(N),A(N,N),B(N,N)
      DIMENSION BUF(100)
      occb=5
      ENTRY INDNS (A,B)
   40 READ(IREAD) BUF
      STOP
      END
