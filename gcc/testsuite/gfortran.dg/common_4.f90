! { dg-do run }
! Suppress warnings about misaligned common blocks.
! { dg-options "-w" }
! Check misaligned common blocks.
program prog
  common /block/ a, b, c
  integer*1 a
  integer*4 b, c
  a = 1
  b = HUGE(b)
  c = 2
  call foo
end program
subroutine foo
  common /block/ a, b, c
  integer*1 a
  integer*4 b, c
  if (a .ne. 1 .or. b .ne. HUGE(b) .or. c .ne. 2) call abort
end subroutine
