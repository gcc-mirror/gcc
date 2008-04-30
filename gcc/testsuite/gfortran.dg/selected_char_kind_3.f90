! { dg-do compile }
! { dg-options "-std=f95 -pedantic -Wall" }
!
! Check that SELECTED_CHAR_KIND is rejected with -std=f95
!
  implicit none
  character(kind=selected_char_kind("ascii")) :: s ! { dg-error "is not included in the selected standard" }
  s = "" ! { dg-error "has no IMPLICIT type" }
  print *, s
end
