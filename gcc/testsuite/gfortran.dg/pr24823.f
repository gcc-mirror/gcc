!     { dg-do compile }
!     { dg-options "-O2 -std=legacy" }
!     PR24823 Flow didn't handle a PARALLEL as destination of a SET properly.
      SUBROUTINE ZLATMR( M, N, DIST, ISEED, SYM, D, MODE, COND, DMAX,
     $     RSIGN, GRADE, DL, MODEL, CONDL, DR, MODER,
     $     PACK, A, LDA, IWORK, INFO )
      COMPLEX*16         A( LDA, * ), D( * ), DL( * ), DR( * )
      LOGICAL            BADPVT, DZERO, FULBND
      COMPLEX*16         ZLATM2, ZLATM3
      IF( IGRADE.EQ.4 .AND. MODEL.EQ.0 ) THEN
      END IF
      IF( IPVTNG.GT.0 ) THEN
      END IF
      IF( M.LT.0 ) THEN
      ELSE IF( IPACK.EQ.-1 .OR. ( ( IPACK.EQ.1 .OR. IPACK.EQ.2 .OR.
     $        IPACK.EQ.5 .OR. IPACK.EQ.6 ) .AND. ISYM.EQ.1 ) .OR.
     $        ( IPACK.EQ.3 .AND. ISYM.EQ.1 .AND. ( KL.NE.0 .OR. M.NE.
     $        6 ) .AND. LDA.LT.KUU+1 ) .OR.
     $        ( IPACK.EQ.7 .AND. LDA.LT.KLL+KUU+1 ) ) THEN
         INFO = -26
      END IF
      IF( INFO.NE.0 ) THEN
         RETURN
      END IF
      IF( KUU.EQ.N-1 .AND. KLL.EQ.M-1 )
     $     FULBND = .TRUE.
      IF( MODE.NE.0 .AND. MODE.NE.-6 .AND. MODE.NE.6 ) THEN
         TEMP = ABS( D( 1 ) )
         IF( TEMP.EQ.ZERO .AND. DMAX.NE.CZERO ) THEN
            INFO = 2
         END IF
      END IF
      IF( ISYM.EQ.0 ) THEN
      END IF
      IF( IGRADE.EQ.1 .OR. IGRADE.EQ.3 .OR. IGRADE.EQ.4 .OR. IGRADE.EQ.
     $     5 .OR. IGRADE.EQ.6 ) THEN
         IF( INFO.NE.0 ) THEN
         END IF
      END IF
      IF( FULBND ) THEN
         IF( IPACK.EQ.0 ) THEN
            IF( ISYM.EQ.0 ) THEN
               CTEMP = ZLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $              IWORK, SPARSE )
               DO 120 I = 1, M
                  CTEMP = ZLATM3( M, N, I, J, ISUB, JSUB, KL, KU,
     $                 IWORK, SPARSE )
 120           CONTINUE
            END IF
            IF( I.LT.1 ) THEN
               IF( ISYM.EQ.0 ) THEN
                  A( J-I+1, I ) = DCONJG( ZLATM2( M, N, I, J, KL,
     $                 DR, IPVTNG, IWORK, SPARSE ) )  ! { dg-warning "Type mismatch" }
               ELSE
                  A( J-I+1, I ) = ZLATM2( M, N, I, J, KL, KU,  ! { dg-warning "Type mismatch" }
     $                 IPVTNG, IWORK, SPARSE )
               END IF
            END IF
            IF( ISYM.NE.1 ) THEN
               IF( I.GE.1 .AND. I.NE.J ) THEN
                  IF( ISYM.EQ.0 ) THEN
                  END IF
               END IF
               A( I-J+KUU+1, J ) = ZLATM2( M, N, I, J, KL, KU,
     $              DR, IPVTNG, IWORK, SPARSE )
            END IF
         END IF
      END IF
      IF( IPACK.EQ.0 ) THEN
         ONORM = ZLANGB( 'M', N, KLL, KUU, A, LDA, TEMPA )
      END IF
      IF( ANORM.GE.ZERO ) THEN
         IF( ANORM.GT.ZERO .AND. ONORM.EQ.ZERO ) THEN
            IF( IPACK.LE.2 ) THEN
            END IF
         END IF
      END IF
      END
