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
    integer, pointer :: dpc1 => r   ! { dg-error "is REAL but should be INTEGER" }
    integer, pointer :: dpc2 => v   ! { dg-error "rank of the element.*does not match" }
    integer, pointer :: dpc3 => i   ! { dg-error "should be a POINTER or a TARGET" }
    integer, pointer :: dpc4 => j   ! { dg-error "must have the SAVE attribute" }
    integer, pointer :: dpc5 => a   ! { dg-error "must not be ALLOCATABLE" }
  end type

  type(t) ::u

end subroutine
