! { dg-do compile }
!
! PR 46161: [OOP] Invalid: Passing non-polymorphic to allocatable polymorphic dummy
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  implicit none

  type :: base
  end type 

  type, extends(base) :: ext
  end type 

  type(base), allocatable :: a
  class(base), pointer :: b
  class(ext), allocatable :: c
  
  call test(a)  ! { dg-error "must be polymorphic" }
  call test(b)  ! { dg-error "must be ALLOCATABLE" }
  call test(c)  ! { dg-error "must have the same declared type" }

contains

  subroutine test(arg)
    implicit none 
    class(base), allocatable :: arg
  end subroutine

end
