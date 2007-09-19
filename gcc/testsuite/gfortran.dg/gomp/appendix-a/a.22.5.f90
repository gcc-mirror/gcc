! { dg-do compile }
! { dg-require-effective-target tls_native }

      SUBROUTINE A22_5_WRONG()
        COMMON /T/ A
!$OMP THREADPRIVATE(/T/)
        CONTAINS
          SUBROUTINE A22_5S_WRONG()
!$OMP PARALLEL COPYIN(/T/)	! { dg-error "COMMON block" }
      !non-conforming because /T/ not declared in A22_5S_WRONG
!$OMP END PARALLEL		! { dg-error "Unexpected" }
          END SUBROUTINE A22_5S_WRONG
      END SUBROUTINE A22_5_WRONG
