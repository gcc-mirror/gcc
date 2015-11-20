! { dg-do compile }
!
! Test the fix for PR68237 in which 'foo' caused a seg fault rather than an error.
!
! Contributed by Martin Reinecke  <martin@mpa-garching.mpg.de>
!
module m1
  interface
    module subroutine bar
    end subroutine
  end interface
end module m1

submodule (m1) m2
contains
  module procedure foo ! { dg-error "must be in a generic module interface" }
  end procedure ! { dg-error "Expecting END SUBMODULE statement" }
end submodule
