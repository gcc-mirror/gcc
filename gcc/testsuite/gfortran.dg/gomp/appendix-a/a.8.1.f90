! { dg-do compile }
      SUBROUTINE A8(N, M, A, B, Y, Z)
         INTEGER N, M
         REAL A(*), B(*), Y(*), Z(*)
         INTEGER I
!$OMP PARALLEL
!$OMP DO
         DO I=2,N
           B(I) = (A(I) + A(I-1)) / 2.0
         ENDDO
!$OMP END DO NOWAIT
!$OMP DO
         DO I=1,M
           Y(I) = SQRT(Z(I))
         ENDDO
!$OMP END DO NOWAIT
!$OMP END PARALLEL
      END SUBROUTINE A8
