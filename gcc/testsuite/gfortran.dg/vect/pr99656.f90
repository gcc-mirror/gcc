! { dg-do compile { target { aarch64*-*-* } } }
! { dg-require-effective-target le }
! { dg-additional-options "-march=armv8.3-a -O1 -ftree-slp-vectorize" }

SUBROUTINE ZLAHQR2(H, LDH, H22, T1)

      INTEGER            LDH
      COMPLEX*16         H(LDH, *)

      INTEGER            NR
      COMPLEX*16         H22, SUM, T1, V2

      COMPLEX*16         V( 3 )

      EXTERNAL           ZLARFG
      INTRINSIC          DCONJG

      V2 = H22
      CALL ZLARFG(T1)
      SUM = DCONJG(T1) * H(1, 1)
      H(1, 1) = SUM * V2

      RETURN
END
