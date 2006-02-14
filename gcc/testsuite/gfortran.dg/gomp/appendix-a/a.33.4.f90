! { dg-do compile }
        SUBROUTINE S(N)
        INTEGER N
        REAL, DIMENSION(:), ALLOCATABLE :: A
        REAL, DIMENSION(:), POINTER :: B
        ALLOCATE (A(N))
!$OMP SINGLE            ! { dg-error "COPYPRIVATE clause object 'a'" }
            ALLOCATE (B(N))
        READ (11) A,B
!$OMP END SINGLE COPYPRIVATE(A,B)
        ! Variable A designates a private object
        !   which has the same value in each thread
        ! Variable B designates a shared object
!$OMP BARRIER
!$OMP SINGLE
          DEALLOCATE (B)
!$OMP END SINGLE NOWAIT
      END SUBROUTINE S

