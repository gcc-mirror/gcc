! { dg-do compile }
!
! PR 46952: [OOP] Spurious "recursive call" error with type bound procedure
!
! Contributed by Ian Harvey <ian_harvey@bigpond.com>

module m

  type, abstract :: t
  contains
    procedure(inter), pass, deferred :: foo
  end type

contains

  subroutine inter(this)
    class(t) :: this
    call this%foo()
  end subroutine inter

end module m 
