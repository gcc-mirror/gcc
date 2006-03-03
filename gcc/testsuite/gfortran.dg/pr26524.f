C PR tree-optimization/26524
C { dg-do compile }
C { dg-options "-O2 -ffast-math" }
      SUBROUTINE CLATM5( PRTYPE, M, N, A, LDA, B, LDB, C, LDC, D, LDD,
     $                   E, LDE, F, LDF, R, LDR, L, LDL, ALPHA, QBLCKA,
     $                   QBLCKB )
      COMPLEX            A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   L( LDL, * ), R( LDR, * )
      COMPLEX            IMEPS, REEPS
         DO 240 I = 1, M
               IF( MOD( I, 2 ).NE.0 .AND. I.LT.M ) THEN
                  A( I, I-1 ) = -IMEPS*2
               END IF
  240    CONTINUE
      END

