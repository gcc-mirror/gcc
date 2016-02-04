! { dg-do compile }
! { dg-options "-O3" }
MODULE qs_efield_berry
  TYPE cp_error_type
  END TYPE
  INTEGER, PARAMETER :: dp=8
  TYPE qs_energy_type
    REAL(KIND=dp), POINTER :: efield
  END TYPE
  TYPE qs_environment_type
  END TYPE
  INTERFACE 
    SUBROUTINE foo(qs_env,energy,error)
       IMPORT 
       TYPE(qs_environment_type), POINTER :: qs_env
       TYPE(cp_error_type)      :: error
       TYPE(qs_energy_type), POINTER   :: energy
    END SUBROUTINE
  END INTERFACE
CONTAINS
  SUBROUTINE qs_efield_mo_derivatives()
    TYPE(qs_environment_type), POINTER :: qs_env
    TYPE(cp_error_type)  :: error
    COMPLEX(dp)          ::   zi(3), zphase(3)
    REAL(dp)             :: ci(3)
    TYPE(qs_energy_type), POINTER      :: energy
    CALL foo(qs_env, energy, error)
    zi = zi * zphase
    ci = AIMAG(LOG(zi))
    DO idir=1,3
       ener_field=ener_field+ci(idir)*fieldfac(idir)
    END DO
    energy%efield=ener_field
  END SUBROUTINE qs_efield_mo_derivatives
END MODULE qs_efield_berry
