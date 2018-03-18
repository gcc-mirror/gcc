! { dg-do compile }
! { dg-additional-options "-Wall -O3" }
! PR 79929 - this used to give a warning.
! Test case by Harald Anlauf.
subroutine gfcbug138 (yerrmsg)
  character(*) :: yerrmsg
  yerrmsg = ""
  yerrmsg = "bug: " // yerrmsg
end subroutine gfcbug138
