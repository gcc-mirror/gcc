! { dg-do run }

! PR fortran/38883
! This ICE'd because the temporary-creation in the MVBITS call was wrong.

PROGRAM main
  IMPLICIT NONE

  TYPE inner
    INTEGER :: i
    INTEGER :: j
  END TYPE inner

  TYPE outer
    TYPE(inner) :: comp(2)
  END TYPE outer

  TYPE(outer) :: var

  var%comp%i = (/ 1, 2 /)
  var%comp%j = (/ 3, 4 /)

  CALL foobar (var, 1, 2)

  IF (ANY (var%comp%i /= (/ 1, 2 /))) CALL abort ()
  IF (ANY (var%comp%j /= (/ 3, 4 /))) CALL abort ()

CONTAINS

  SUBROUTINE foobar (x, lower, upper)
    TYPE(outer), INTENT(INOUT) :: x
    INTEGER, INTENT(IN) :: lower, upper
    CALL MVBITS (x%comp%i, 1, 2, x%comp(lower:upper)%i, 1)
  END SUBROUTINE foobar

END PROGRAM main
