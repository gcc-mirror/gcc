! PR tree-optimization/29581
! { dg-do run }
! { dg-options "-O2 -ftree-loop-linear -fdump-tree-graphite-details --param graphite-allow-codegen-errors=1" }

      SUBROUTINE FOO (K)
      INTEGER I, J, K, A(5,5), B
      COMMON A
      A(1,1) = 1
 10   B = 0
      DO 30 I = 1, K
        DO 20 J = 1, K
          B = B + A(I,J)
 20     CONTINUE
        A(I,I) = A(I,I) * 2
 30   CONTINUE
      IF (B.GE.3) RETURN
      GO TO 10
      END SUBROUTINE

      PROGRAM BAR
        INTEGER A(5,5)
        COMMON A
        CALL FOO (2)
        IF (A(1,1).NE.8) CALL ABORT
        A(1,1) = 0
        IF (ANY(A.NE.0)) CALL ABORT
      END

! { dg-final { scan-tree-dump-times "code generation error" 1 "graphite" } }
