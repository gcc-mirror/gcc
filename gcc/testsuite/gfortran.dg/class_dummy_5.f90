! { dg-do compile }
!
! PR 54756: [OOP] [F08] Should reject CLASS, intent(out) in PURE procedures
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
  type t
  contains
    final :: fnl   ! impure finalizer
  end type t
contains
  impure subroutine fnl(x)
    type(t) :: x
    print *,"finalized!"
  end subroutine
end

program test
  use m
  type(t) :: x
  call foo(x)
contains
  pure subroutine foo(x)  ! { dg-error "may not be polymorphic" }
    ! pure subroutine would call impure finalizer
    class(t), intent(out) :: x
  end subroutine
end

! { dg-final { cleanup-modules "m" } }
