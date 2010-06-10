! { dg-do run }
!
! Check that runtime result values of SELECTED_CHAR_KIND agree with
! front-end simplification results.
!
  implicit none
  character(len=20) :: s

  s = "ascii"
  if (selected_char_kind(s) /= selected_char_kind("ascii")) call abort

  s = "default"
  if (selected_char_kind(s) /= selected_char_kind("default")) call abort

  s = "iso_10646"
  if (selected_char_kind(s) /= selected_char_kind("iso_10646")) call abort

  s = ""
  if (selected_char_kind(s) /= selected_char_kind("")) call abort

  s = "invalid"
  if (selected_char_kind(s) /= selected_char_kind("invalid")) call abort

end
