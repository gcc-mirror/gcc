! { dg-do compile }
!
! PR 45290: [F08] pointer initialization
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

subroutine sub
  implicit none

  real, target, save :: r
  integer, target, save, dimension(1:3) :: v

  integer, save :: i
  integer, target :: j
  integer, target, save, allocatable :: a


  integer, pointer :: dp0 => 13  ! { dg-error "Error in pointer initialization" }
  integer, pointer :: dp1 => r   ! { dg-error "Different types in pointer assignment" }
  integer, pointer :: dp2 => v   ! { dg-error "Different ranks in pointer assignment" }
  integer, pointer :: dp3 => i   ! { dg-error "is neither TARGET nor POINTER" }
  integer, pointer :: dp4 => j   ! { dg-error "must have the SAVE attribute" }
  integer, pointer :: dp5 => a   ! { dg-error "must not be ALLOCATABLE" }

  type :: t
    integer, pointer :: dpc0 => 13  ! { dg-error "Error in pointer initialization" }
  end type t

  type t2
    integer, pointer :: dpc1 => r   ! { dg-error "attempted assignment of REAL.4. to INTEGER.4." }
  end type t2

  type t3
    integer, pointer :: dpc2 => v   ! { dg-error "Different ranks in pointer assignment" }
  end type t3

  type t4
    integer, pointer :: dpc3 => i   ! { dg-error "Pointer assignment target is neither TARGET nor POINTER" }
  end type t4

  type t5
    integer, pointer :: dpc4 => j   ! { dg-error "must have the SAVE attribute" }
  end type t5

  type t6
    integer, pointer :: dpc5 => a   ! { dg-error "must not be ALLOCATABLE" }
  end type t6

end subroutine
