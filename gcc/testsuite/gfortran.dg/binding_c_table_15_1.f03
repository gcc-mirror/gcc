! { dg-do run }
! Test the named constants in Table 15.1.
program a
  use, intrinsic :: iso_c_binding
  implicit none
  if (C_NULL_CHAR       /=  CHAR(0) ) STOP 1
  if (C_ALERT           /= ACHAR(7) ) STOP 2
  if (C_BACKSPACE       /= ACHAR(8) ) STOP 3
  if (C_FORM_FEED       /= ACHAR(12)) STOP 4
  if (C_NEW_LINE        /= ACHAR(10)) STOP 5
  if (C_CARRIAGE_RETURN /= ACHAR(13)) STOP 6
  if (C_HORIZONTAL_TAB  /= ACHAR(9) ) STOP 7
  if (C_VERTICAL_TAB    /= ACHAR(11)) STOP 8
end program a
