! { dg-do run }
! Test the fix for PR61406
! Contributed by Adam Hirst  <adam@aphirst.karoo.co.uk>
program test
  implicit none
  real :: theta = 1.0

  associate (n => [cos(theta), sin(theta)])
    if (abs (norm2(n) - 1.0) .gt. 1.0e-4) call abort
  end associate

end program test
