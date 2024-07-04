! { dg-do run }
! Test fix for PR114141
! Contributed by Steve Kargl  <sgk@troutmask.apl.washington.edu>
program foo
   implicit none
   real :: y = 0.0
   associate (x => log(cmplx(-1,0)))
      y = x%im  ! Gave 'Symbol ‘x’ at (1) has no IMPLICIT type'
      if (int(100*y)-314 /= 0) stop 1
   end associate

! Check wrinkle in comment 1 (parentheses around selector) of the PR is fixed.
   associate (x => ((log(cmplx(-1,1)))))
      y = x%im  ! Gave 'The RE or IM part_ref at (1) must be applied to a
                ! COMPLEX expression'
      if (int(100*y)-235 /= 0) stop 2
   end associate

! Check that more complex(pun intended!) expressions are OK.
   associate (x => exp (log(cmplx(-1,0))+cmplx(0,0.5)))
      y = x%re  ! Gave 'Symbol ‘x’ at (1) has no IMPLICIT type'
      if (int(1000*y)+877 /= 0) stop 3
   end associate

! Make sure that AIMAG intrinsic is OK.
   associate (x => ((log(cmplx(-1,0.5)))))
      y = aimag (x)
      if (int(100*y)-267 /= 0) stop 4
   end associate
end program
