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

     if (allocated (x)) call abort()
     if (.not. same_type_as(x, var_t)) call abort()

     allocate (t2 :: x(5))
  end subroutine sub

  subroutine sub2(x)
     class(t), allocatable, OPTIONAL, intent(out) :: x(:)

     if (.not. present(x)) return
     if (allocated (x)) call abort()
     if (.not. same_type_as(x, var_t)) call abort()

     allocate (t2 :: x(5))
  end subroutine sub2
end module m

use m
implicit none
class(t), save, allocatable :: y(:)

if (allocated (y)) call abort()
if (.not. same_type_as(y,var_t)) call abort()

call sub(y)
if (.not.allocated(y)) call abort()
if (.not. same_type_as(y, var_t2)) call abort()
if (size (y) /= 5) call abort()

call sub(y)
if (.not.allocated(y)) call abort()
if (.not. same_type_as(y, var_t2)) call abort()
if (size (y) /= 5) call abort()

deallocate (y)
if (allocated (y)) call abort()
if (.not. same_type_as(y,var_t)) call abort()

call sub2()

call sub2(y)
if (.not.allocated(y)) call abort()
if (.not. same_type_as(y, var_t2)) call abort()
if (size (y) /= 5) call abort()

call sub2(y)
if (.not.allocated(y)) call abort()
if (.not. same_type_as(y, var_t2)) call abort()
if (size (y) /= 5) call abort()
end

! { dg-final { scan-tree-dump-times "__builtin_free" 5 "original" } }
! { dg-final { scan-tree-dump-times "finally" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
