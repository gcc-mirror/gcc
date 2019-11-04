! { dg-do compile }
! { dg-options "-O1 -ftree-loop-vectorize -fno-signed-zeros -fno-trapping-math" }
! { dg-additional-options "-mvsx" { target { powerpc*-*-* } } }
      COMPLEX FUNCTION R1 (ZR, CC, EA, U6)

      INTEGER ZR, U6, FZ, J2
      COMPLEX EA(*), CC
      DOUBLE PRECISION OS, GA, YU, XT

      OS = DBLE(REAL(CC))
      GA = DBLE(AIMAG(CC))
      J2 = 1

      DO 5 FZ = 1, ZR
        YU = DBLE(REAL(EA(J2)))
        XT = DBLE(AIMAG(EA(J2)))
        OS = OS + (YU * 2) - (XT * 2)
        GA = GA + (YU * 3) + (XT * 3)
        J2 = J2 + U6
    5 CONTINUE
      R1 = CMPLX(REAL(OS), REAL(GA))
      RETURN
      END
