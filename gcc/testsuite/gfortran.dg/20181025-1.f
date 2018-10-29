! { dg-do compile }
! { dg-options "-Ofast" }
! { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } }
      SUBROUTINE FOO(EF3,CA,ZA,NATA,IC4,NFRGPT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXATM=500)
      COMMON DE(3,MXATM)
      DIMENSION CA(3,NATA)
      DIMENSION ZA(NATA)
      DIMENSION EF3(3,NFRGPT)
      DO II = 1,NATA
         XII = XJ - CA(1,II)
         YII = YJ - CA(2,II)
         ZII = ZJ - CA(3,II)
         RJII = SQRT(XII*XII + YII*YII + ZII*ZII)
         R3 = RJII*RJII*RJII
         IF (IC4.EQ.0) THEN
            DE(1,II) = DE(1,II) - S2*ZA(II)*XII/R3
            DE(2,II) = DE(2,II) - S2*ZA(II)*YII/R3
            DE(3,II) = DE(3,II) - S2*ZA(II)*ZII/R3
         ELSE 
            EF3(1,IC4+II) = EF3(1,IC4+II) - S2*ZA(II)*XII/R3
            EF3(2,IC4+II) = EF3(2,IC4+II) - S2*ZA(II)*YII/R3
            EF3(3,IC4+II) = EF3(3,IC4+II) - S2*ZA(II)*ZII/R3
         END IF
      END DO
      RETURN
      END           
