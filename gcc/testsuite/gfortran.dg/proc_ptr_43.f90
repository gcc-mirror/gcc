! { dg-do compile }
!
! PR 58099: [4.8/4.9 Regression] [F03] over-zealous procedure-pointer error checking
!
! Contributed by Daniel Price <daniel.price@monash.edu>

  implicit none
  procedure(real), pointer :: wfunc

  wfunc => w_cubic 
 
contains

  pure real function w_cubic(q2)
    real, intent(in) :: q2
    w_cubic = 0.
  end function

end
