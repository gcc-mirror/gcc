! { dg-do run }
! Test the named constants in Table 15.1.
program a
  use, intrinsic :: iso_c_binding
  implicit none
  if (C_NULL_CHAR       /=  CHAR(0) ) call abort
  if (C_ALERT           /= ACHAR(7) ) call abort
  if (C_BACKSPACE       /= ACHAR(8) ) call abort
  if (C_FORM_FEED       /= ACHAR(12)) call abort
  if (C_NEW_LINE        /= ACHAR(10)) call abort
  if (C_CARRIAGE_RETURN /= ACHAR(13)) call abort
  if (C_HORIZONTAL_TAB  /= ACHAR(9) ) call abort
  if (C_VERTICAL_TAB    /= ACHAR(11)) call abort
end program a
