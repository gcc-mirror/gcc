! { dg-do compile }
! { dg-options "-frepack-arrays " }
!
! Original class_array_11.f03 but with -frepack-arrays a new
! ICE was produced reported in
! PR fortran/69659
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

