C Derived from lapack
      SUBROUTINE ZGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, RWORK, INFO )
      COMPLEX*16         WORK( * )
            DO 20 I = 1, RANK
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
      END
