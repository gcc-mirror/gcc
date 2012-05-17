! { dg-do compile }
! Verifies that the c_loc scalar pointer tests recognize the string of length
! one as being allowable for the parameter to c_loc.
module x
use iso_c_binding
contains
SUBROUTINE glutInit_f03()
  TYPE(C_PTR), DIMENSION(1), TARGET :: argv=C_NULL_PTR
  CHARACTER(C_CHAR), DIMENSION(10), TARGET :: empty_string=C_NULL_CHAR
  argv(1)=C_LOC(empty_string)
END SUBROUTINE
end module x
