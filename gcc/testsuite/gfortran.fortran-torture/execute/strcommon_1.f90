! PR14081 character variables in common blocks.

subroutine test1
  implicit none
  common /block/ c
  character(len=12) :: c

  if (c .ne. "Hello World") call abort
end subroutine

subroutine test2
  implicit none
  common /block/ a
  character(len=6), dimension(2) :: a

  if ((a(1) .ne. "Hello") .or. (a(2) .ne. "World")) call abort
end subroutine

program strcommon_1
  implicit none
  common /block/ s, t
  character(len=6) :: s, t
  s = "Hello "
  t = "World "
  call test1
  call test2
end program

