! { dg-do compile }
! { dg-options "-O3 -ffast-math" }

      SUBROUTINE ZUNG2L( M, N, K, A, LDA, TAU, WORK, INFO )
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
      IF( M.LT.0 ) THEN
      END IF
         CALL ZLARF( 'LEFT', M-N+II, II-1, A( 1, II ), 1, TAU( I ), A,
     $               LDA, WORK )
         CALL ZSCAL( M-N+II-1, -TAU( I ), A( 1, II ), 1 )
         A( M-N+II, II ) = ONE - TAU( I )
      END
      SUBROUTINE ZLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
      CHARACTER         SIDE(*)
      LOGICAL            LSAME
      COMPLEX*16         C( LDC, * ), V(*), WORK(*), TAU
      IF( LSAME( SIDE, 'L' ) ) THEN
         IF( TAU.NE.ZERO ) THEN
            CALL ZGEMV( 'CONJUGATE TRANSPOSE', M, N, ONE, C, LDC, V,
     $                  INCV, ZERO, WORK, 1 )
         END IF
      END IF
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
      CHARACTER         CA(*), CB(*)
      END
      SUBROUTINE ZGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
      CHARACTER         TRANS(*)
      LOGICAL            LSAME
      IF( LSAME( TRANS, 'N' ) )THEN
         IF( INCY.EQ.1 )THEN
               IF( X( JX ).NE.ZERO )THEN
                     Y( I ) = Y( I ) + TEMP*A( I, J )
               END IF
         END IF
      END IF
      END
      SUBROUTINE  ZSCAL(N,ZA,ZX,INCX)
      COMPLEX*16 ZA,ZX(1)
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN
   20 DO 30 I = 1,N
        ZX(I) = ZA*ZX(I)
   30 CONTINUE
      END
