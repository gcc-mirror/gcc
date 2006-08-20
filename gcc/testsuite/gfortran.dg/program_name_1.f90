! { dg-do compile }
! Tests the fix for PR28762 in which the program name would cause
! the compiler to test the write statement as a variable thereby generating
! an "Expecting VARIABLE" error.
!
! Contributed by David Ham  <David@ham.dropbear.id.au>
!
program write
  integer :: debuglevel = 1
  if (0 < debuglevel) write (*,*) "Hello World"
end program write
