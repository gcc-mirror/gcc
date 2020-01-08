! { dg-do compile }
! { dg-options "-Warray-bounds -O2" }
! PR 92422 - this complained about an array subscript out of bounds.

PROGRAM character_warning

  CHARACTER(len=16) :: word

  word = 'hi'
  WRITE(*,*) word

END PROGRAM character_warning
