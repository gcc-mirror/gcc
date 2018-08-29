! { dg-do compile }
!
! PR 47710: [OOP] Improve ambiguity check for GENERIC TBP w/ PASS and NOPASS
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

  type base_t
  contains
    procedure, nopass :: baseproc_nopass => baseproc1
    procedure, pass   :: baseproc_pass => baseproc2
    generic           :: some_proc => baseproc_pass, baseproc_nopass   ! { dg-error "are ambiguous" }
  end type

contains

  subroutine baseproc1 (this)
    class(base_t) :: this
  end subroutine

  subroutine baseproc2 (this, that)
    class(base_t) :: this, that
  end subroutine

end module
