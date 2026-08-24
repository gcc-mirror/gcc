! { dg-do run }
! PR 49802
! The caller-side copy made for an assumed-length CHARACTER VALUE dummy
! copied a number of bytes equal to the character count, which is wrong
! for a character kind wider than one byte.

program test
  implicit none
  character(kind=4,len=10) :: s4
  character(kind=1,len=10) :: s1

  s4 = 4_"abcdefghij"
  call by_value_k4 (s4)
  if (s4 /= 4_"abcdefghij") stop 1

  s1 = "abcdefghij"
  call by_value_k1 (s1)
  if (s1 /= "abcdefghij") stop 2

contains

  subroutine by_value_k4 (y)
    character(kind=4,len=*), value :: y
    if (len (y) /= 10) stop 3
    if (y /= 4_"abcdefghij") stop 4
    y = 4_"ZZZZZZZZZZ"
  end subroutine

  subroutine by_value_k1 (y)
    character(kind=1,len=*), value :: y
    if (y /= "abcdefghij") stop 5
    y = "ZZZZZZZZZZ"
  end subroutine

end program
