! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
!
use iso_fortran_env
implicit none

type t1
  type(lock_type), allocatable :: x[:]
end type t1

type t2
  type(lock_type) :: x
end type t2

type(t1) :: a
type(t2) :: b[*]
!class(lock_type), allocatable :: cl[:]

lock(a%x) ! { dg-error "the lock component of derived type at \\(1\\) is not yet supported" }
lock(b%x) ! { dg-error "the lock component of derived type at \\(1\\) is not yet supported" }
!lock(cl)

unlock(a%x) ! { dg-error "the lock component of derived type at \\(1\\) is not yet supported" }
unlock(b%x) ! { dg-error "the lock component of derived type at \\(1\\) is not yet supported" }
!unlock(cl)
end
