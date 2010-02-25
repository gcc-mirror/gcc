C PR debug/43166
C { dg-do compile }
C { dg-options "-O" }
      SUBROUTINE FOO ()
      INTEGER V1
      COMMON // V1
      END
      SUBROUTINE BAR ()
      INTEGER V0,V1,V2,V3
      COMMON // V1(4),V2(85,4),V3
      DO V3=1,V1(1)
      V0=V2(V3,1)
      END DO
      END
