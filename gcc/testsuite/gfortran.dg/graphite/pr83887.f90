! { dg-do compile }
! { dg-options "-O -floop-nest-optimize" }
      SUBROUTINE ZTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA, &
                   B, LDB )
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      complex(kind((1.0d0,1.0d0)))         ALPHA
      complex(kind((1.0d0,1.0d0)))         A( LDA, * ), B( LDB, * )
      EXTERNAL           XERBLA
      INTRINSIC          CONJG, MAX
      LOGICAL            LSIDE, NOCONJ, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      complex(kind((1.0d0,1.0d0)))         TEMP
      complex(kind((1.0d0,1.0d0)))         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      complex(kind((1.0d0,1.0d0)))         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
      LSIDE  =  scan( SIDE  , 'Ll' )>0
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOCONJ =  scan( TRANSA, 'Tt' )>0
      NOUNIT =  scan( DIAG  , 'Nn' )>0
      UPPER  =  scan( UPLO  , 'Uu' )>0
      INFO   = 0
      IF( N.EQ.0 ) &
   RETURN
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
               DO 160, J = 1, N
                  DO 150, I = 1, M
                     TEMP = B( I, J )
                     IF( NOCONJ )THEN
                        IF( NOUNIT ) &
                     TEMP = TEMP*A( I, I )
                        DO 130, K = I + 1, M
                           TEMP = TEMP + A( K, I )*B( K, J )
  130                   CONTINUE
                     ELSE
                        IF( NOUNIT ) &
                     TEMP = TEMP*CONJG( A( I, I ) )
                        DO 140, K = I + 1, M
                           TEMP = TEMP + CONJG( A( K, I ) )*B( K, J )
  140                   CONTINUE
                     END IF
                     B( I, J ) = ALPHA*TEMP
  150             CONTINUE
  160          CONTINUE
      RETURN
      END

