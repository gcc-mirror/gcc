! pr 15149
! verify the random number generator is functional
      program test_random
      implicit none
      real :: r(5) = 0.0

      call random_number(r)
      if (all (r .eq. 0)) call abort
      end program


