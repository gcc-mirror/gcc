! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
module m
  implicit none
  type t
  end type t

  type, extends(t) :: t2
  end type t2

  type(t) :: var_t
  type(t2) :: var_t2
contains
  subroutine sub(x)
     class(t), allocatable, intent(out) :: x(:)

     if (allocated (x)) STOP 1
     if (.not. same_type_as(x, var_t)) STOP 2

     allocate (t2 :: x(5))
  end subroutine sub

  subroutine sub2(x)
     class(t), allocatable, OPTIONAL, intent(out) :: x(:)

     if (.not. present(x)) return
     if (allocated (x)) STOP 3
     if (.not. same_type_as(x, var_t)) STOP 4

     allocate (t2 :: x(5))
  end subroutine sub2
end module m

use m
implicit none
class(t), save, allocatable :: y(:)

if (allocated (y)) STOP 5
if (.not. same_type_as(y,var_t)) STOP 6

call sub(y)
if (.not.allocated(y)) STOP 7
if (.not. same_type_as(y, var_t2)) STOP 8
if (size (y) /= 5) STOP 9

call sub(y)
if (.not.allocated(y)) STOP 10
if (.not. same_type_as(y, var_t2)) STOP 11
if (size (y) /= 5) STOP 12

deallocate (y)
if (allocated (y)) STOP 13
if (.not. same_type_as(y,var_t)) STOP 14

call sub2()

call sub2(y)
if (.not.allocated(y)) STOP 15
if (.not. same_type_as(y, var_t2)) STOP 16
if (size (y) /= 5) STOP 17

call sub2(y)
if (.not.allocated(y)) STOP 18
if (.not. same_type_as(y, var_t2)) STOP 19
if (size (y) /= 5) STOP 20
end

! { dg-final { scan-tree-dump-times "__builtin_free" 5 "original" } }
! { dg-final { scan-tree-dump-times "finally" 0 "original" } }
