! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Different CHARACTER lengths" }
! Tests the fix for PR34396, where the non-constant string lengths in the
! array constructor were being ignored and the bounds checking was not
! being done correctly.
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
program array_char
  implicit none
  integer :: i, j(5)
  character (len=5) :: x, y
  character (len=5) :: z(2)
  x = "ab"
  y = "cd"
  z = ""
  z = (/y(1: len (trim(y))), x(1: len (trim(x)))/)
  j = ichar ([(z(1)(i:i), i=1,5)])
  if (any (j .ne. (/99,100,32,32,32/))) STOP 1
  j = ichar ([(z(2)(i:i), i=1,5)])
  if (any (j .ne. (/97,98,32,32,32/))) STOP 2
  x = "a "
  z = (/y(1: len (trim(y))), x(1: len (trim(x)))/)
end program array_char

! { dg-output "At line 24 of file .*" }
! { dg-output "Different CHARACTER lengths .2/1. in array constructor" }
