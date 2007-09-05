! { dg-do run }
! Tests the fix for PR31564, in which the actual argument to
! the call for set_bound was simplified when it should not be.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
MODULE cdf_aux_mod
  TYPE :: the_distribution
    INTEGER :: parameters(2)
  END TYPE the_distribution
  TYPE (the_distribution), PARAMETER :: the_beta = the_distribution((/99,999/))
CONTAINS
  SUBROUTINE set_bound(arg_name, test)
    INTEGER, INTENT (IN) :: arg_name, test
    if (arg_name .ne. test) call abort ()
  END SUBROUTINE set_bound
END MODULE cdf_aux_mod

MODULE cdf_beta_mod
CONTAINS
  SUBROUTINE cdf_beta(which, test)
    USE cdf_aux_mod
    INTEGER :: which, test
    CALL set_bound(the_beta%parameters(which), test)
  END SUBROUTINE cdf_beta
END MODULE cdf_beta_mod

  use cdf_beta_mod
  call cdf_beta (1, 99)
  call cdf_beta (2, 999)
end
! { dg-final { cleanup-modules "cdf_aux_mod cdf_beta_mod" } }
