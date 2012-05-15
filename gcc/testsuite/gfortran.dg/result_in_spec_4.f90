! { dg-do compile }
!
! PR fortran/49648
! ICE for calls to a use-associated function returning an array whose spec
! depends on a function call.

! Contributed by Tobias Burnus <burnus@net-b.de>

module m2
  COMPLEX, SAVE, ALLOCATABLE :: P(:)
contains
  FUNCTION getPhaseMatrix() RESULT(PM)
    COMPLEX:: PM(SIZE(P),3)
    PM=0.0
  END FUNCTION
end module m2

module m
  use m2
contains
   SUBROUTINE gf_generateEmbPot()
      COMPLEX :: sigma2(3,3)
      sigma2 = MATMUL(getPhaseMatrix(), sigma2)
   END SUBROUTINE
end module m
