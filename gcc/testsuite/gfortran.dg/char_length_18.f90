! { dg-do compile }
! PR 45576 - no ICE for missing optional argument
! Test case supplied by Joost VandeVondele
SUBROUTINE get_r_val()
  INTEGER, PARAMETER :: default_string_length=128
  CHARACTER(len=default_string_length) :: c_val
  LOGICAL                              :: check 
  check = c_val(LEN_TRIM(c_val):LEN_TRIM(c_val))=="]"
END SUBROUTINE get_r_val
