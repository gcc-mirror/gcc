! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
!
! LOCK/LOCK_TYPE checks 
!

subroutine valid()
  use iso_fortran_env
  implicit none
  type t
    type(lock_type) :: lock
  end type t

  type t2
    type(lock_type), allocatable :: lock(:)[:]
  end type t2

  type(t), save :: a[*]
  type(t2), save :: b ! OK

  allocate(b%lock(1)[*])
  LOCK(a%lock) ! OK
  LOCK(a[1]%lock) ! OK

  LOCK(b%lock(1)) ! OK
  LOCK(b%lock(1)[1]) ! OK
end subroutine valid

subroutine invalid()
  use iso_fortran_env
  implicit none
  type t
    type(lock_type) :: lock
  end type t
  type(t), save :: a ! { dg-error "type LOCK_TYPE or with subcomponent of type LOCK_TYPE must be a coarray" }
end subroutine invalid

subroutine more_tests
  use iso_fortran_env
  implicit none
  type t
    type(lock_type) :: a ! OK
  end type t

  type t1
    type(lock_type), allocatable :: c2(:)[:] ! OK 
  end type t1
  type(t1) :: x1 ! OK

  type t2
    type(lock_type), allocatable :: c1(:) ! { dg-error "Allocatable component c1 at .1. of type LOCK_TYPE must have a codimension" }
  end type t2

  type t3
    type(t) :: b
  end type t3
  type(t3) :: x3 ! { dg-error "of type LOCK_TYPE or with subcomponent of type LOCK_TYPE must be a coarray" }

  type t4
    type(lock_type) :: c0(2)
  end type t4
  type(t4) :: x4 ! { dg-error "of type LOCK_TYPE or with subcomponent of type LOCK_TYPE must be a coarray" }
end subroutine more_tests
