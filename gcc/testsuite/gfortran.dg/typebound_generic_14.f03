! { dg-do compile }
!
! PR 54594: [OOP] Type-bound ASSIGNMENTs (elemental + array version) rejected as ambiguous
!
! Contributed by James van Buskirk

module a_mod

  type :: a
   contains
     procedure, NOPASS :: a_ass, a_ass_sv
     generic :: ass => a_ass, a_ass_sv
  end type

contains

  impure elemental subroutine a_ass (out)
    class(a), intent(out) :: out
  end subroutine

  subroutine a_ass_sv (out)
    class(a), intent(out) :: out(:)
  end subroutine

end module
