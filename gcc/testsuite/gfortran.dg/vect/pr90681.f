C { dg-do compile }
C { dg-additional-options "-march=armv8.2-a+sve" { target { aarch64*-*-* } } }
      SUBROUTINE HMU (H1,NORBS)
      COMMON DD(107)
      DIMENSION H1(NORBS,*)
            DO 70 J1 = IA,I1
               H1(I1,J1) = 0
               JO1 = J1
               IF (JO1.EQ.1) THEN
                   H1(I1,J1) = DD(NI)
               END IF
   70       CONTINUE
      END
