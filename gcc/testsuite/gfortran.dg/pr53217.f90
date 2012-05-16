! { dg-do compile }
! { dg-options "-O3 -ffast-math" }

! This tests only for compile-time failure, which formerly occurred
! when statements were emitted out of order, failing verify_ssa.

MODULE xc_cs1
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
  REAL(KIND=dp), PARAMETER :: a = 0.04918_dp, &
                              c = 0.2533_dp, &
                              d = 0.349_dp
CONTAINS
  SUBROUTINE cs1_u_2 ( rho, grho, r13, e_rho_rho, e_rho_ndrho, e_ndrho_ndrho,&
       npoints, error)
    REAL(KIND=dp), DIMENSION(*), &
      INTENT(INOUT)                          :: e_rho_rho, e_rho_ndrho, &
                                                e_ndrho_ndrho
    DO ip = 1, npoints
      IF ( rho(ip) > eps_rho ) THEN
         oc = 1.0_dp/(r*r*r3*r3 + c*g*g)
         d2rF4 = c4p*f13*f23*g**4*r3/r * (193*d*r**5*r3*r3+90*d*d*r**5*r3 &
                 -88*g*g*c*r**3*r3-100*d*d*c*g*g*r*r*r3*r3 &
                 +104*r**6)*od**3*oc**4
         e_rho_rho(ip) = e_rho_rho(ip) + d2F1 + d2rF2 + d2F3 + d2rF4
      END IF
    END DO
  END SUBROUTINE cs1_u_2
END MODULE xc_cs1
