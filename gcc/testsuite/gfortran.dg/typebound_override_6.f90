! { dg-do compile }
!
! PR 54190: TYPE(*)/assumed-rank: Type/rank check too relaxed for dummy procedure
! PR 57217: [4.7/4.8/4.9 Regression][OOP] Accepts invalid TBP overriding - lacking arguments check
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module base_mod
  implicit none
  type base_type
    integer :: kind
  contains
    procedure, pass(map)  :: clone    => base_clone
  end type
contains
  subroutine  base_clone(map,mapout,info)
    class(base_type), intent(inout) :: map
    class(base_type), intent(inout) :: mapout
    integer     :: info
  end subroutine
end module

module r_mod
  use base_mod
  implicit none
  type, extends(base_type) :: r_type
    real  :: dat
  contains
    procedure, pass(map)  :: clone    => r_clone   ! { dg-error "Rank mismatch in argument" }
  end type
contains
  subroutine  r_clone(map,mapout,info)
    class(r_type), intent(inout) :: map
    class(base_type), intent(inout) :: mapout(..)
    integer     :: info
  end subroutine
end module
