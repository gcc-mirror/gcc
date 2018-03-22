c { dg-do run }
      DIMENSION A(-5:5)
      INTEGER(kind=1) IM5, IZ, IP5
      INTEGER(kind=2) IM1, IP1
      PARAMETER (IM5=-5, IM1=-1, IZ=0, IP1=1, IP5=5)
      DATA A(IM5) /-5./, A(IM1) /-1./
      DATA A(IZ)  /0./
      DATA A(IP5) /+5./, A(IP1) /+1./
      IF (A(IM5) .NE. -5. .OR. A(IM1) .NE. -1. .OR.
     ,    A(IZ)  .NE.  0. .OR.
     ,    A(IP5) .NE. +5. .OR. A(IP1) .NE. +1. )
     ,  STOP 1
      END
