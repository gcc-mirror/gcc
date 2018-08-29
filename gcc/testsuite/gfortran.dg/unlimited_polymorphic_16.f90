! { dg-do compile }
!
! PR 60359: [OOP] symbol `__io_MOD___copy_character_1' is already defined
!
! Contributed by Antony Lewis <antony@cosmologist.info>

module IO
implicit none

contains

  subroutine FWRite(S)
    class(*) :: S
  end subroutine

  subroutine IO_OutputMargeStats()
    character(len=128) tag
    call FWrite(tag)
    call FWrite(' '//tag)
  end subroutine

end module
