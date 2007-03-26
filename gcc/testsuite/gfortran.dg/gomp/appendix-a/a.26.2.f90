! { dg-do compile }

       MODULE A26_2
       REAL A
       CONTAINS
         SUBROUTINE G(K)
           REAL K
           A = K ! This is A in module A26_2, not the private
                  ! A in F
         END SUBROUTINE G
         SUBROUTINE F(N)
         INTEGER N
         REAL A
            INTEGER I
!$OMP PARALLEL DO PRIVATE(A)
              DO I = 1,N
                A=I
                CALL G(A*2)
              ENDDO
!$OMP END PARALLEL DO
          END SUBROUTINE F
      END MODULE A26_2
! { dg-final { cleanup-modules "A26_2" } }
