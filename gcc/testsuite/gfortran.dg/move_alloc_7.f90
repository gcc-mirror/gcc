! { dg-do compile }
!
! Check that move alloc handles different, type compatible
! declared types
!
type t
end type t
type, extends(t) :: t2
end type t2

class(t), allocatable :: x
class(t2), allocatable :: y
allocate(y)
call move_alloc (y, x)
end
