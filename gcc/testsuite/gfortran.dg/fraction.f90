! { dg-do run }
!
! Test for pr52413
!

program test_frac

  real :: y
  y=fraction (-2.0) 
  if (fraction (-2.0) /= -0.5) call abort ()
  if (fraction (-0.0) /= 0.0) call abort ()
  if (sign(1.0, fraction(-0.0)) /= -1.0) call abort ()
  if (fraction (-2.0_8) /=  -0.5) call abort ()

end program test_frac
