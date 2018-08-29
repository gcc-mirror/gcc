! { dg-do compile }
!
! PR 53718: [4.7/4.8 regression] [OOP] gfortran generates asm label twice in the same output file
!
! Contributed by Adrian Prantl <adrian@llnl.gov>

module m
  type t
  end type
end module

subroutine sub1
  use m
  class(t), pointer :: a1
end subroutine

subroutine sub2
  use m
  class(t), pointer :: a2
end subroutine
