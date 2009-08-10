! { dg-do compile }
!
! FIXME: Remove -w after polymorphic entities are supported.
! { dg-options "-w" }
!
! PR 40940: CLASS statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

use,intrinsic :: iso_c_binding

type t1
  integer :: comp
end type

type t2
  sequence
  real :: r
end type

type,bind(c) :: t3
  integer(c_int) :: i
end type

type :: t4
  procedure(absint), pointer :: p  ! { dg-error "Non-polymorphic passed-object dummy argument" }
end type

type :: t5
  class(t1) :: c  ! { dg-error "must be allocatable or pointer" }
end type

abstract interface
  subroutine absint(arg)
    import :: t4
    type(t4) :: arg
  end subroutine
end interface


class(t1) :: o1  ! { dg-error "must be dummy, allocatable or pointer" }

class(t2), pointer :: o2  ! { dg-error "is not extensible" }
class(t3), pointer :: o3  ! { dg-error "is not extensible" }

end

