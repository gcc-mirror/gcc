C PR tree-optimization/39516
C { dg-do compile }
C { dg-options "-O2 -ftree-loop-linear" }
      SUBROUTINE SUB(A, B, M)
      IMPLICIT NONE
      DOUBLE PRECISION A(20,20), B(20)
      INTEGER*8 I, J, K, M
      DO I=1,M
        DO J=1,M
          A(I,J)=A(I,J)+1
        END DO
      END DO
      DO K=1,20
        DO I=1,M
          DO J=1,M
            B(I)=B(I)+A(I,J)
          END DO
        END DO
      END DO
      END SUBROUTINE
