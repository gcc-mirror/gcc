! { dg-do run }
! Tests the fix for PR38907, in which any expressions, including unary plus,
! in front of the call to S_REAL_SUM_I (marked) would throw the mechanism
! for correcting invalid host association.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
module sa0054_stuff
  REAL :: S_REAL_SUM_2(10) = [(REAL (I), I = 1, 10)]
contains
  ELEMENTAL FUNCTION S_REAL_SUM_I (A)
    REAL  ::  S_REAL_SUM_I
    REAL, INTENT(IN)  ::  A
    X = 1.0
    S_REAL_SUM_I = X
  END FUNCTION S_REAL_SUM_I
  SUBROUTINE SA0054 (RDA)
    REAL RDA(:)
    RDA =  + S_REAL_SUM_I (RDA)          ! Reported problem => ICE
    RDA = RDA + S_REAL_SUM_2 (INT (RDA)) ! Also failed
  CONTAINS
    ELEMENTAL FUNCTION S_REAL_SUM_I (A)
      REAL  ::  S_REAL_SUM_I
      REAL, INTENT(IN)  ::  A
      S_REAL_SUM_I = 2.0 * A
    END FUNCTION S_REAL_SUM_I
    ELEMENTAL FUNCTION S_REAL_SUM_2 (A)
      REAL  ::  S_REAL_SUM_2
      INTEGER, INTENT(IN)  ::  A
      S_REAL_SUM_2 = 2.0 * A
    END FUNCTION S_REAL_SUM_2
  END SUBROUTINE
end module sa0054_stuff

  use sa0054_stuff
  REAL :: RDA(10) = [(REAL(I), I = 1, 10)]
  call SA0054 (RDA)
  IF (ANY (INT (RDA) .ne. [(6 * I, I = 1, 10)])) print *, rda
END
