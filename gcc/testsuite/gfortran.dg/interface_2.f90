! { dg-do compile }
! PR fortran/24545
MODULE Compare_Float_Numbers

  IMPLICIT NONE

  INTERFACE Compare_Float
    MODULE PROCEDURE Compare_Float_Single
  END INTERFACE Compare_Float

  INTERFACE OPERATOR (.EqualTo.)
    MODULE PROCEDURE Is_Equal_To_Single
  END INTERFACE OPERATOR (.EqualTo.)

CONTAINS

  FUNCTION Is_Equal_To_Single(x, y) RESULT(Equal_To)
    REAL(4), INTENT(IN) :: x, y
    LOGICAL :: Equal_To
    Equal_To = .true.
  END FUNCTION Is_Equal_To_Single

  FUNCTION Compare_Float_Single(x, y) RESULT(Compare)
    REAL(4), INTENT(IN) :: x, y
    LOGICAL :: Compare
    Compare = .true.
  END FUNCTION Compare_Float_Single

END MODULE Compare_Float_Numbers
