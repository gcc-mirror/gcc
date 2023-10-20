! { dg-do compile }
! { dg-options "-O2" }
! { dg-additional-options "-mavx" { target avx } }

!GCC$ builtin (powf) attributes simd (notinbranch) if('x86_64')

PARAMETER (NX=3, G=1.4)
DIMENSION T(NX,NX), P(NX,NX)
INTEGER Apx
COMMON P, T

DO i = 1, 3
  IF  (i < 0.0 ) THEN
     P(Apx,i) = i**G
     T(Apx,i) = i**G
  ELSE
     P(Apx,i) = 0
     T(Apx,i) = 0
  ENDIF
ENDDO
END
