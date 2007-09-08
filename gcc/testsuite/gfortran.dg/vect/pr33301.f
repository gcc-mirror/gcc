c { dg-do compile }
C Derived from lapack
      SUBROUTINE ZGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, RWORK, INFO )
      COMPLEX(kind=8)         WORK( * )
c     Following declaration added on transfer to gfortran testsuite.
c     It is present in original lapack source
      integer rank
            DO 20 I = 1, RANK
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
      END

c { dg-final { cleanup-tree-dump "vect" } }
