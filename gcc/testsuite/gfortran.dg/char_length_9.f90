! { dg-do compile }
! Test the fix for a regression caused by the first fix of PR31879.
! Reported by Tobias Burnus <burnus@gcc.gnu.org>
!
MODULE input_val_types
  IMPLICIT NONE
  INTEGER, PARAMETER :: default_string_length=80
  TYPE val_type
    CHARACTER(len=default_string_length), DIMENSION(:), POINTER :: c_val
  END TYPE val_type
CONTAINS
  SUBROUTINE val_get (val, c_val)
    TYPE(val_type), POINTER                  :: val
    CHARACTER(LEN=*), INTENT(out)            :: c_val
    INTEGER                                  :: i, l_out
    i=1
    c_val((i-1)*default_string_length+1:MIN (l_out, i*default_string_length)) = &
               val%c_val(i)(1:MIN (80, l_out-(i-1)*default_string_length))
  END SUBROUTINE val_get
END MODULE input_val_types

! { dg-final { cleanup-modules "input_val_types" } }
