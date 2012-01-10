! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/51605
!

subroutine one()
type t
end type t
! (a) Invalid (was ICEing before)
class(t), target :: p1 ! { dg-error "must be dummy, allocatable or pointer" }
class(t), pointer :: p2

select type(p1)
  type is(t)
    p2 => p1
  class is(t)
    p2 => p1
end select
end subroutine one

subroutine two()
type t
end type t
class(t), allocatable, target :: p1 ! (b) Valid
class(t), pointer :: p2

select type(p1)
  type is(t)
    p2 => p1
  class is(t)
    p2 => p1
end select
end subroutine two

subroutine three()
type t
end type t
class(t), allocatable :: p1         ! (c) Invalid as not TARGET
class(t), pointer :: p2

select type(p1)
  type is(t)
    p2 => p1 ! { dg-error "Pointer assignment target is neither TARGET nor POINTER" }
  class is(t)
    p2 => p1 ! { dg-error "Pointer assignment target is neither TARGET nor POINTER" }
end select
end subroutine three

subroutine four()
type t
end type t
class(t), pointer :: p1             ! (d) Valid
class(t), pointer :: p2

select type(p1)
  type is(t)
    p2 => p1
  class is(t)
    p2 => p1
end select
end subroutine four

subroutine caf(x)
  type t
  end type t
  class(t) :: x[*]
  select type(x)
  type is(t)
  end select
end subroutine caf
