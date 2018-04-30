! { dg-do compile }
!
! PR 53328: [OOP] Ambiguous check for type-bound GENERIC shall ignore PASSed arguments
!
! Contributed by Salvatore Filippone <filippone.salvatore@gmail.com>

module m
  type t
  contains
    procedure, pass(this) :: sub1
    procedure, pass(this) :: sub2
    generic :: gen => sub1, sub2   ! { dg-error "are ambiguous" }
  end type t
contains
  subroutine sub1 (x, this)
    integer :: i
    class(t) :: this
  end subroutine sub1

  subroutine sub2 (this, y)
    integer :: i
    class(t) :: this
  end subroutine sub2
end module m 
