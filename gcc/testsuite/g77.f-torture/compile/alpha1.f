      REAL*8 A,B,C
      REAL*4 RARRAY(19)/19*(-1)/
      INTEGER BOTTOM,RIGHT
      INTEGER IARRAY(19)/0,0,0,0,0,0,0,0,0,0,0,0,13,14,0,0,0,0,0/
      EQUIVALENCE (RARRAY(13),BOTTOM),(RARRAY(14),RIGHT)
C
      IF(I.NE.0) call exit(1)
C gcc: Internal compiler error: program f771 got fatal signal 11
C  at this point!
      END
