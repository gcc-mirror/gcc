! { dg-do compile }
!
! PR 42677: [4.5 Regression] Bogus Error: Ambiguous interfaces '...' in intrinsic assignment operator
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

module mod1
  implicit none
  type t_m
     integer :: i = 0
  end type t_m
!------------------------------------------------------------------------------
  interface assignment (=)
     module procedure assign_m
  end interface
!------------------------------------------------------------------------------
contains
  subroutine assign_m (y, x)
    type(t_m) ,intent(inout) :: y
    type(t_m) ,intent(in)    :: x
  end subroutine assign_m
end module mod1
!==============================================================================
module mod2
  use mod1, only: t_m, assignment(=)
  implicit none
  type t_atm
     integer :: k
  end type t_atm
!------------------------------------------------------------------------------
  interface assignment(=)
     module procedure assign_to_atm
  end interface
!------------------------------------------------------------------------------
  interface
     pure subroutine delete_m (x)
       use mod1
       type(t_m) ,intent(in) :: x
     end subroutine delete_m
  end interface
!------------------------------------------------------------------------------
contains
  subroutine assign_to_atm (atm, r)
    type(t_atm) ,intent(inout) :: atm
    integer     ,intent(in)    :: r
  end subroutine assign_to_atm
end module mod2
 
! { dg-final { cleanup-modules "mod1 mod2" } }
