! Program to test initialization of common blocks.
subroutine test()
  character(len=15) :: c
  integer d, e
  real f
  common /block2/ c
  common /block/ d, e, f

  if ((d .ne. 42) .or. (e .ne. 43) .or. (f .ne. 2.0)) call abort ()
  if (c .ne. "Hello World    ") call abort ()
end subroutine

program prog
  integer a(2)
  real b
  character(len=15) :: s
  common /block/ a, b
  common /block2/ s
  data b, a/2.0, 42, 43/
  data s /"Hello World"/

  call test ()
end program

