! { dg-do run }
!
! Check that runtime result values of SELECTED_CHAR_KIND agree with
! front-end simplification results.
!
  implicit none
  character(len=20) :: s

  s = "ascii"
  if (selected_char_kind(s) /= selected_char_kind("ascii")) STOP 1

  s = "default"
  if (selected_char_kind(s) /= selected_char_kind("default")) STOP 2

  s = "iso_10646"
  if (selected_char_kind(s) /= selected_char_kind("iso_10646")) STOP 3

  s = ""
  if (selected_char_kind(s) /= selected_char_kind("")) STOP 4

  s = "invalid"
  if (selected_char_kind(s) /= selected_char_kind("invalid")) STOP 5

end
