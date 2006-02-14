! { dg-do compile }

      SUBROUTINE A30(N, A, B)
       INTEGER N
       REAL A(*), B(*)
        INTEGER I
!$OMP PARALLEL
!$OMP DO LASTPRIVATE(I)
        DO I=1,N-1
          A(I) = B(I) + B(I+1)
        ENDDO
!$OMP END PARALLEL
        A(I) = B(I)      ! I has the value of N here
      END SUBROUTINE A30
