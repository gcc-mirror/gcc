! { dg-options "-fgraphite-identity -g -O3 -ffast-math" }
MODULE erf_fn
CONTAINS
  SUBROUTINE CALERF(ARG,RESULT,JINT)
    DIMENSION A(5),B(4),C(9),D(8),P(6),Q(5)
    IF (Y <= THRESH) THEN
       DO I = 1, 3
          XNUM = (XNUM + A(I)) * YSQ
          XDEN = (XDEN + B(I)) * YSQ
       END DO
       RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
    END IF
  END SUBROUTINE CALERF
END MODULE erf_fn
! { dg-final { cleanup-modules "erf_fn" } }
