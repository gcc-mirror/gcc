! { dg-do compile }
!
! PR 127541
! PR 53653

subroutine s
  type t
    integer :: i
  end type
  type, extends(t) :: t2
  end type
  class(t), allocatable :: x, z(:)
  class(t2), allocatable :: u
  type(t2) :: y
  z = [x, y] ! { dg-error "Element in TYPE\\(t\\) array constructor" }
  z = [y, x] ! { dg-error "Element in TYPE\\(t2\\) array constructor" }
  z = [x, u] ! { dg-error "Element in TYPE\\(t\\) array constructor" }
end subroutine

subroutine s2
  type t
    integer :: c
  end type
  class(t) :: v ! { dg-error "must be dummy, allocatable or pointer" }
  integer :: n
  n = size ([v, t(2)])
end subroutine
