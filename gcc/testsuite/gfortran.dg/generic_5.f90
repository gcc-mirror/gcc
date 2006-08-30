! { dg-do compile }
! Tests the patch for PR28201, in which the call to ice would cause an ICE
! because resolve.c(resolve_generic_s) would try to look in the parent
! namespace to see if the subroutine was part of a legal generic interface.
! In this case, there is nothing to test, hence the ICE.
!
! Contributed by Daniel Franke  <franke.daniel@gmail.com>
!
!
MODULE ice_gfortran
  INTERFACE ice
    MODULE PROCEDURE ice_i
  END INTERFACE

CONTAINS
  SUBROUTINE ice_i(i)
    INTEGER, INTENT(IN) :: i
    ! do nothing
  END SUBROUTINE
END MODULE

MODULE provoke_ice
CONTAINS
  SUBROUTINE provoke
    USE ice_gfortran
    CALL ice(23.0)   ! { dg-error "no specific subroutine" }
  END SUBROUTINE
END MODULE
! { dg-final { cleanup-modules "ice_gfortran provoke_ice" } }
