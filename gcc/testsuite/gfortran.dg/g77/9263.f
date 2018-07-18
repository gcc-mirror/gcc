C { dg-do compile }
      PARAMETER (Q=1)
      PARAMETER (P=10)
      INTEGER C(10),D(10),E(10),F(10)
C     TERMINAL NOT INTEGER
      DATA (C(I),I=1,P)    /10*10/ ! { dg-error "End expression in DO loop" }
C     START NOT INTEGER
      DATA (D(I),I=Q,10)   /10*10/ ! { dg-error "Start expression in DO loop" }
C     INCREMENT NOT INTEGER
      DATA (E(I),I=1,10,Q) /10*10/ ! { dg-error "Step expression in DO loop" }
      END
