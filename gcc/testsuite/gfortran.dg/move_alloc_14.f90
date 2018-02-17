! { dg-do run }
!
! Ensure that move_alloc for CLASS resets the FROM variable's dynamic type
! to the declared one
!
implicit none
type t
end type t
type, extends(t) :: t2
end type t2

class(t), allocatable :: a, b, c
class(t), allocatable :: a2(:), b2(:), c2(:)
allocate (t2 :: a)
allocate (t2 :: a2(5))
call move_alloc (from=a, to=b)
call move_alloc (from=a2, to=b2)
!print *, same_type_as (a,c), same_type_as (a,b)
!print *, same_type_as (a2,c2), same_type_as (a2,b2)
if (.not. same_type_as (a,c) .or. same_type_as (a,b)) STOP 1
if (.not. same_type_as (a2,c2) .or. same_type_as (a2,b2)) STOP 2
end
