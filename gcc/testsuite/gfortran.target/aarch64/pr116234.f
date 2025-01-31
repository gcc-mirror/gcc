! { dg-do compile }
! { dg-options "-fcompare-debug -mcpu=phecda -O2 -funroll-all-loops -c -fno-rename-registers -fno-ivopts" }

      SUBROUTINE FOO(UPLO, N, A, IA, JA, DESCA, SR, SC, SCOND, AMAX,
     $     EQUED)

      CHARACTER          EQUED, UPLO
      INTEGER            IA, JA
      DOUBLE PRECISION   AMAX, SCOND

      INTEGER            DESCA(*)
      DOUBLE PRECISION   A(*), SR(*)

      INTEGER            IACOL, IAROW, IC, IIA, IOFFA, J, JB, JJ, KK,
     $     LL, MYCOL, MYROW
      DOUBLE PRECISION   CJ, SMALL
      DOUBLE PRECISION   SC(*)

      EXTERNAL           INFOG2L

      LOGICAL            BAR
      DOUBLE PRECISION   PDLAMCH
      EXTERNAL           BAR, PDLAMCH

      INTRINSIC          MOD

      CALL INFOG2L(IA, JA, DESCA, MYROW, MYCOL, IIA, IAROW, IACOL)

      CJ = 1
      SMALL = PDLAMCH(IC, 'Safe minimum') / PDLAMCH(IC, 'Precision')

      IF (SCOND .LT. 0.1D+0 .OR. AMAX .LT. SMALL) THEN
         JJ = LL
         JB = LL + 1

         IF (BAR(UPLO, '0')) THEN
            IF (MYCOL .EQ. IACOL) THEN
               DO 10 LL = 1, JB
                  IOFFA = IOFFA + LL
 10            CONTINUE
            END IF

            DO 60 J = 1, JA + 1
               IF (MYCOL .EQ. IACOL) THEN
                  IF (MYROW .EQ. IAROW) THEN
                     DO 30 LL = JJ, JJ + JB - 1
                        CJ = SC(LL)
                        DO 20 KK = IIA, MYROW + LL - JJ + 1
                           A(IOFFA + KK) = A(IOFFA + KK) * CJ * SR(KK)
 20                     CONTINUE
 30                  CONTINUE
                  ELSE
                     DO 50 LL = JJ, JJ + JB
                        DO 40 KK = IIA, J
                           A(IOFFA + KK) = A(IOFFA + KK) * CJ * SR(KK)
 40                     CONTINUE
                        IOFFA = IOFFA + KK
 50                  CONTINUE
                  END IF
               END IF

               IACOL = MOD(IACOL + 1, JA)
 60         CONTINUE
         ELSE
            IF (MYROW .NE. IAROW) THEN
               DO 70 LL = 1, JB
                  A(IOFFA + KK) = A(IOFFA + KK) * CJ
 70            CONTINUE
            END IF

            DO 90 J = 1, JA
               DO 80 LL = 1, JJ
                  A(IOFFA + KK) = A(IOFFA + KK) * CJ
 80            CONTINUE
 90         CONTINUE
         END IF
      END IF

      RETURN
      END
