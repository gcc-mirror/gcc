! { dg-do compile }
! { dg-options "-O2 -fpeel-loops -ftree-loop-vectorize -fno-tree-scev-cprop --param iv-max-considered-uses=2" }
      REAL               FUNCTION FOO(M, N, A, W)

      INTEGER            M, N

      REAL               W(*)
      COMPLEX            A(*)

      INTEGER            II, JI, JJ, KK, LL, MP

      EXTERNAL           BAR

      INTEGER            QUX
      EXTERNAL           QUX

      CALL BAR(II)

      IF (M .EQ. 0) THEN
         IF (N .EQ. 0) THEN
            DO 140 KK = II, II + MP
               W(KK) = 0
 140        CONTINUE
         ELSE
            KK = II + MP
         END IF

         DO 130 JI = KK, KK + MP
            DO 120 LL = JJ, JJ + MP
               DO 110 KK = II, II + MP
                  W(KK) = (A(KK))
 110           CONTINUE
 120        CONTINUE
 130     CONTINUE

         IF (W(KK) .EQ. 0) THEN
            FOO = W(QUX(MP, W, 1))
         END IF

      END IF

      RETURN

      END
