! { dg-do run }
! Tests the fix for PR33499 in which the ENTRY cx_radc was not
! getting its TYPE.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
MODULE complex
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: cx, OPERATOR(+), OPERATOR(.eq.)
  TYPE cx
    integer :: re
    integer :: im
  END TYPE cx
  INTERFACE OPERATOR (+)
    MODULE PROCEDURE cx_cadr, cx_radc
  END INTERFACE
  INTERFACE OPERATOR (.eq.)
    MODULE PROCEDURE cx_eq
  END INTERFACE
  CONTAINS
  FUNCTION cx_cadr(z, r)
  ENTRY cx_radc(r, z)
    TYPE (cx) :: cx_cadr, cx_radc
    TYPE (cx), INTENT(IN) :: z
    integer, INTENT(IN) :: r
    cx_cadr%re = z%re + r
    cx_cadr%im = z%im
  END FUNCTION cx_cadr
  FUNCTION cx_eq(u, v)
    TYPE (cx), INTENT(IN) :: u, v
    logical :: cx_eq
    cx_eq = (u%re .eq. v%re) .and. (u%im .eq. v%im)
  END FUNCTION cx_eq
END MODULE complex

  use complex
  type(cx) :: a = cx (1, 2), c, d
  logical :: f
  integer :: b = 3
  if (.not.((a + b) .eq. (b + a))) call abort ()
  if (.not.((a + b) .eq. cx (4, 2))) call abort ()
end
! { dg-final { cleanup-modules "complex" } }
