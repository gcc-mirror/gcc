! { dg-do run }
! PR17706
! this is a libgfortran test
! output value -0.00 is not standard compliant
! derived from NIST F77 test FM406, with extra bits added.
program pr17706
  implicit none
  character(len=10) :: s
  character(len=10), parameter :: x = "xxxxxxxxxx"
  real, parameter :: small = -0.0001

  s = x
  write (s, '(F4.1)') small
  ! The plus is optional.  We choose not to display it.
  if (s .ne. " 0.0") call abort

  s = x
  write (s, '(SS,F4.1)') small
  if (s .ne. " 0.0") call abort

  s = x
  write (s, '(SP,F4.1)') small
  if (s .ne. "+0.0") call abort
end program
