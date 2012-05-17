! { dg-do run }
! Tests the fix for PR33337, which was partly associated with
! the problem in PR31564 and, in addition, the parentheses in
! the initialization expression for the_chi_square.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
MODULE cdf_nc_chisq_mod
  PUBLIC
  TYPE :: one_parameter
    INTEGER :: high_bound
  END TYPE one_parameter
  TYPE :: the_distribution
    TYPE (one_parameter) :: parameters(1)
  END TYPE the_distribution
  TYPE (the_distribution), PARAMETER :: the_chi_square = &
    the_distribution((/(one_parameter(99))/))
CONTAINS
  SUBROUTINE local_cum_nc_chisq()
    integer :: df0
    df0 = the_chi_square%parameters(1)%high_bound
    print *, df0
  END SUBROUTINE local_cum_nc_chisq
END MODULE cdf_nc_chisq_mod

  use cdf_nc_chisq_mod
  call local_cum_nc_chisq
end
