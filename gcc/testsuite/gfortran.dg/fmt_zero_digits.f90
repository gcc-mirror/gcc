! { dg-do run }
! Verify that when decimal precision is zero, no error.
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program test
  character(20) :: astr
  50 FORMAT (d20.0)
  astr = ""
  write(astr,50) -8.0D0
  if (astr.ne."             -0.D+01") call abort()
  write(astr,50) 8.0D0
  if (astr.ne."              0.D+01") call abort()
end program test
