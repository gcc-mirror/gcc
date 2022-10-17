C { dg-do compile }
C { dg-options "-std=legacy" }
      SUBROUTINE DTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
     $                   LDVR, MM, M, WORK, INFO )
      DOUBLE PRECISION   T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WORK( * )
      DOUBLE PRECISION   X( 2, 2 )
      CALL DLALN2( .FALSE., 1, 1, SMIN, ONE, T( J, J ),
     $                            ZERO, X, 2, SCALE, XNORM, IERR ) ! { dg-warning "Type mismatch" }
      CALL DSCAL( KI, SCALE, WORK( 1+N ), 1 )
      DO 90 J = KI - 2, 1, -1
      IF( J.GT.JNXT )
     $               GO TO 90
      JNXT = J - 1
      IF( J.GT.1 ) THEN
          IF( T( J, J-1 ).NE.ZERO ) THEN
              IF( WORK( J ).GT.BIGNUM / XNORM ) THEN
                  X( 1, 1 ) = X( 1, 1 ) / XNORM
              END IF
          END IF
          CALL DLALN2( .FALSE., 2, 2, SMIN, ONE,
     $                            T( J-1, J-1 ), LDT, ONE, ONE,  ! { dg-warning "Type mismatch" }
     $                            XNORM, IERR )  ! { dg-warning "Type mismatch" }
          CALL DAXPY( J-2, -X( 1, 1 ), T( 1, J-1 ), 1,
     $                           WORK( 1+N ), 1 )
          CALL DAXPY( J-2, -X( 2, 2 ), T( 1, J ), 1,
     $                           WORK( 1+N2 ), 1 )
      END IF
   90          CONTINUE
      END
