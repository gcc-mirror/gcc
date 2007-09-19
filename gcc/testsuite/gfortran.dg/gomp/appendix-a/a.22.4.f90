! { dg-do compile }
! { dg-require-effective-target tls_native }

      MODULE A22_MODULE
      COMMON /T/ A
      END MODULE A22_MODULE
      SUBROUTINE A22_4_WRONG()
        USE A22_MODULE
!$OMP THREADPRIVATE(/T/)	! { dg-error "COMMON block" }
      !non-conforming because /T/ not declared in A22_4_WRONG
      END SUBROUTINE A22_4_WRONG
! { dg-final { cleanup-modules "A22_MODULE" } }
