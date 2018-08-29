! { dg-do run }
! Test the verify intrinsic.  We were ignoring the last character.
program prog
  character(len=1) :: c1
  character(len=4) :: c4
  c1 = "E"
  if (verify(c1, "1") .ne. 1) STOP 1
  c4 = "ABBA"
  if (verify(c4, "A") .ne. 2) STOP 2
  if (verify(c4, "A", back = .true.) .ne. 3) STOP 3
  if (verify(c4, "AB") .ne. 0) STOP 4
end program
