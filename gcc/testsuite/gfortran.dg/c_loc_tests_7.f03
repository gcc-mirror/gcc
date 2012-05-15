! { dg-do compile }
module c_loc_tests_7
use iso_c_binding
contains
SUBROUTINE glutInit_f03()
  TYPE(C_PTR), DIMENSION(1), TARGET :: argv=C_NULL_PTR
  CHARACTER(C_CHAR), DIMENSION(1), TARGET :: empty_string=C_NULL_CHAR
  argv(1)=C_LOC(empty_string)
END SUBROUTINE
end module c_loc_tests_7
