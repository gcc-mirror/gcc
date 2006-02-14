! { dg-do compile }

      SUBROUTINE A31_1(A, B, X, Y, N)
        INTEGER N
        REAL X(*), Y(*), A, B
!$OMP PARALLEL DO PRIVATE(I) SHARED(X, N) REDUCTION(+:A)
!$OMP& REDUCTION(MIN:B)
        DO I=1,N
           A = A + X(I)
           B = MIN(B, Y(I))
!  Note that some reductions can be expressed in
!  other forms. For example, the MIN could be expressed as
!  IF (B > Y(I)) B = Y(I)
         END DO
      END SUBROUTINE A31_1
