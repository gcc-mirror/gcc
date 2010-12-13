C PR fortran/46884
C { dg-do compile }
C { dg-options "" }
      SUBROUTINE F
      IMPLICIT CHARACTER*12 (C)
      CALL G(C1)
      CALL H(C1(1:4))
      END
