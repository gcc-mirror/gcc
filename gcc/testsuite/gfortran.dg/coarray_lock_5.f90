! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! LOCK_TYPE checks
!
module m3
  use iso_fortran_env
  type, extends(lock_type) :: lock
    integer :: j = 7
  end type lock
end module m3

use m3
type(lock_type) :: tl[*] = lock_type ()
type(lock) :: t[*]
tl = lock_type () ! { dg-error "variable definition context" }
print *,t%j
end

subroutine test()
  use iso_fortran_env
  type t
    type(lock_type) :: lock
  end type t

  type t2
    type(t), pointer :: x ! { dg-error "Pointer component x at .1. has a noncoarray subcomponent of type LOCK_TYPE, which must have a codimension or be a subcomponent of a coarray" }
  end type t2
end subroutine test

subroutine test2()
  use iso_fortran_env
  implicit none
  type t
    type(lock_type), allocatable :: lock ! { dg-error "Allocatable component lock at .1. of type LOCK_TYPE must have a codimension" }
  end type t
  type t2
    type(lock_type) :: lock
  end type t2
  type t3
    type(t2), allocatable :: lock_cmp
  end type t3
  type t4
    integer, allocatable :: a[:]
    type(t2) :: b ! { dg-error "Noncoarray component b at .1. of type LOCK_TYPE or with subcomponent of type LOCK_TYPE must have a codimension or be a subcomponent of a coarray. .Variables of type t4 may not have a codimension as already a coarray subcomponent exists." }
  end type t4
  type t5
    type(t2) :: c ! { dg-error "Noncoarray component c at .1. of type LOCK_TYPE or with subcomponent of type LOCK_TYPE must have a codimension or be a subcomponent of a coarray. .Variables of type t5 may not have a codimension as d at .2. has a codimension or a coarray subcomponent." }
    integer, allocatable :: d[:] ! { dg-error "Noncoarray component c at .1. of type LOCK_TYPE or with subcomponent of type LOCK_TYPE must have a codimension or be a subcomponent of a coarray. .Variables of type t5 may not have a codimension as d at .2. has a codimension or a coarray subcomponent." }
  end type t5
end subroutine test2

! { dg-final { cleanup-modules "m3" } }
