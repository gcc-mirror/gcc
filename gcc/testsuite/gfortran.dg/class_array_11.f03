! { dg-do compile }
!
! PR fortran/46356
! This program was leading to an ICE related to class arrays
!
! Original testcase by Ian Harvey <ian_harvey@bigpond.com>
! Reduced by Janus Weil <Janus@gcc.gnu.org>

  IMPLICIT NONE

  TYPE :: ParentVector
    INTEGER :: a
  END TYPE ParentVector  

CONTAINS       

  SUBROUTINE vector_operation(pvec)     
    CLASS(ParentVector), INTENT(INOUT) :: pvec(:)
    print *,pvec(1)%a
  END SUBROUTINE

END

