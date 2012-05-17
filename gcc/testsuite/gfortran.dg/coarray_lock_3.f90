! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
!
! LOCK/LOCK_TYPE checks 
!
subroutine extends()
use iso_fortran_env
type t
end type t
type, extends(t) :: t2 ! { dg-error "coarray component, parent type .t. shall also have one" }
  type(lock_type), allocatable :: c(:)[:]
end type t2
end subroutine extends

module m
  use iso_fortran_env

  type t
    type(lock_type), allocatable :: x(:)[:]
  end type t
end module m

module m2
  use iso_fortran_env
  type t2
    type(lock_type), allocatable :: x ! { dg-error "Allocatable component x at .1. of type LOCK_TYPE must have a codimension" }
  end type t2
end module m2

module m3
  use iso_fortran_env
  type t3
    type(lock_type) :: x ! OK
  end type t3
end module m3

subroutine sub(x)
  use iso_fortran_env
  type(lock_type), intent(out) :: x[*] ! OK
end subroutine sub

subroutine sub1(x) ! { dg-error "is INTENT.OUT. and can thus not be an allocatable coarray or have coarray components" }
  use iso_fortran_env
  type(lock_type), allocatable, intent(out) :: x(:)[:]
end subroutine sub1

subroutine sub2(x) ! { dg-error "is INTENT.OUT. and can thus not be an allocatable coarray or have coarray components" }
  use m
  type(t), intent(out) :: x
end subroutine sub2

subroutine sub3(x) ! { dg-error "with coarray component shall be a nonpointer, nonallocatable scalar" }
  use m
  type(t), intent(inout) :: x[*]
end subroutine sub3

subroutine sub4(x)
  use m3
  type(t3), intent(inout) :: x[*] ! OK
end subroutine sub4

subroutine lock_test
  use iso_fortran_env
  type t
  end type t
  type(lock_type) :: lock ! { dg-error "of type LOCK_TYPE or with subcomponent of type LOCK_TYPE must be a coarray" }
end subroutine lock_test

subroutine lock_test2
  use iso_fortran_env
  implicit none
  type t
  end type t
  type(t) :: x
  type(lock_type), save :: lock[*],lock2(2)[*]
  lock(t) ! { dg-error "Syntax error in LOCK statement" }
  lock(x) ! { dg-error "must be a scalar of type LOCK_TYPE" }
  lock(lock)
  lock(lock2(1))
  lock(lock2) ! { dg-error "must be a scalar of type LOCK_TYPE" }
  lock(lock[1]) ! OK
end subroutine lock_test2


subroutine lock_test3
  use iso_fortran_env
  type(lock_type), save :: a[*], b[*]
  a = b ! { dg-error "LOCK_TYPE in variable definition context" }
  b = lock_type() ! { dg-error "LOCK_TYPE in variable definition context" }
  print *, a ! { dg-error "cannot have PRIVATE components" }
end subroutine lock_test3


subroutine lock_test4
  use iso_fortran_env
  type(lock_type), allocatable :: A(:)[:]
  logical :: ob
  allocate(A(1)[*])
  lock(A(1), acquired_lock=ob)
  unlock(A(1))
  deallocate(A)
end subroutine lock_test4


subroutine argument_check()
  use iso_fortran_env
  type(lock_type), SAVE :: ll[*]
  call no_interface(ll) ! { dg-error "Actual argument of LOCK_TYPE or with LOCK_TYPE component at .1. requires an explicit interface" }
  call test(ll) ! { dg-error "non-INTENT.INOUT. dummy .x. at .1., which is LOCK_TYPE or has a LOCK_TYPE component" }
contains
  subroutine test(x)
    type(lock_type), intent(in) :: x[*]
  end subroutine test
end subroutine argument_check
