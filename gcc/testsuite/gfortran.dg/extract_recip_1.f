! { dg-do compile }
! { dg-options "-Ofast -fdump-tree-optimized-raw" }

      SUBROUTINE F(N,X,Y,Z,A,B)
          DIMENSION X(4,4), Y(4), Z(4)
          REAL, INTENT(INOUT) :: A, B

          A = 1 / (Y(N)*Y(N))

          DO I = 1, NV
          X(I, I) = 1 + X(I, I)
          ENDDO

          Z(1) =  B / Y(N)
          Z(2) =  N / Y(N)
          RETURN
      END

! { dg-final { scan-tree-dump-times "rdiv_expr" 1 "optimized" } }
