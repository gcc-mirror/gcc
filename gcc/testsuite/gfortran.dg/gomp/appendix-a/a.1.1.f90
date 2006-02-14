! { dg-do compile }
      SUBROUTINE A1(N, A, B)
      INTEGER I, N
      REAL B(N), A(N)
!$OMP PARALLEL DO !I is private by default
      DO I=2,N
          B(I) = (A(I) + A(I-1)) / 2.0
      ENDDO
!$OMP END PARALLEL DO
      END SUBROUTINE A1
