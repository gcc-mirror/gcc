! { dg-do compile }
! { dg-require-effective-target tls }

      MODULE M
        REAL, POINTER, SAVE :: WORK(:)
        INTEGER :: SIZE
        REAL :: TOL
!$OMP THREADPRIVATE(WORK,SIZE,TOL)
      END MODULE M
      SUBROUTINE A32( T, N )
        USE M
        REAL :: T
        INTEGER :: N
        TOL = T
        SIZE = N
!$OMP PARALLEL COPYIN(TOL,SIZE)
        CALL BUILD
!$OMP END PARALLEL
      END SUBROUTINE A32
      SUBROUTINE BUILD
        USE M
        ALLOCATE(WORK(SIZE))
        WORK = TOL
      END SUBROUTINE BUILD
! { dg-final { cleanup-modules "M" } }
