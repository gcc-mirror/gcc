! { dg-do compile }
! Check the fix for PR32129 in which the argument 'vec(vy(i, :))' was
! incorrectly simplified, resulting in an ICE and a missed error.
!
! Reported by Tobias Burnus <burnus@gcc.gnu.org>
!
    MODULE cdf_aux_mod
      TYPE :: the_distribution
        INTEGER :: parameters(1)
      END TYPE the_distribution
      TYPE (the_distribution), PARAMETER :: the_beta = the_distribution((/0/))
    CONTAINS
      SUBROUTINE set_bound(arg_name)
        INTEGER, INTENT (IN) :: arg_name
      END SUBROUTINE set_bound
    END MODULE cdf_aux_mod
    MODULE cdf_beta_mod
    CONTAINS
      SUBROUTINE cdf_beta()
        USE cdf_aux_mod
        INTEGER :: which
          which = 1
          CALL set_bound(the_beta%parameters(1:which)) ! { dg-error "Rank mismatch" }
      END SUBROUTINE cdf_beta
    END MODULE cdf_beta_mod
