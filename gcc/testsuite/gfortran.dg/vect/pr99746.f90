! { dg-do compile }
! { dg-additional-options "-march=armv8.3-a" { target aarch64-*-* } }
SUBROUTINE CLAREF(A, WANTZ, Z, ICOL1, ITMP1, ITMP2, T1, T2, V2, LDA)
LOGICAL            BLOCK, WANTZ
COMPLEX            T1, T2, V2
COMPLEX            A(LDA, *), VECS, Z(LDA, *)
COMPLEX            SUM
LOGICAL            LSAME
IF (LSAME) THEN
      DO 30 K = ITMP1, ITMP2, 3
         T1 = VECS0
30       CONTINUE
ELSE
   IF (BLOCK) THEN
      DO 90 K = ITMP1, ITMP2 - 1, 3
         A(J, ICOL1) = ITMP1
         IF (WANTZ) THEN
            DO 80 J = ITMP1, ITMP2
               SUM =  ICOL1
               Z(J, 3) = V23
80             CONTINUE
         END IF
90       CONTINUE
      DO 120 K = ITMP1, ITMP2
         V2 = VECS()
         DO 100 J = ITMP1, ITMP2
            A(J, ICOL1) = A(J, ICOL1) - SUM
100          CONTINUE
         IF (WANTZ) THEN
            DO 110 J = 1, 3
               SUM = Z(J, ICOL1)
               Z(J, ICOL1) = 0
110             CONTINUE
         END IF
         ICOL1 = ICOL1 + 1
120       CONTINUE
   ELSE
      DO 130 J = ITMP1, ITMP2
         SUM = T1 * A(J, ICOL1) + T2 * A(J, 1) + V2 * A(J, 2)
         A(J, ICOL1) = SUM
         A(J, ICOL1 + 2) = SUM * V1
130       CONTINUE
   END IF
END IF
END
