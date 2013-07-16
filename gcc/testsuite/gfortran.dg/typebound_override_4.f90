! { dg-do compile }
!
! PR 57217: [4.7/4.8/4.9 Regression][OOP] Accepts invalid TBP overriding - lacking arguments check
!
! Contributed by Salvatore Filippone <filippone.salvatore@gmail.com>

module base_mod
  implicit none
  type base_type
  contains
    procedure, pass(map)  :: clone    => base_clone
  end type
contains
  subroutine  base_clone(map,mapout)
    class(base_type) :: map
    class(base_type) :: mapout
  end subroutine
end module

module r_mod
  use base_mod
  implicit none
  type, extends(base_type) :: r_type
  contains
    procedure, pass(map)  :: clone    => r_clone   ! { dg-error "Type/rank mismatch in argument" }
  end type
contains
  subroutine  r_clone(map,mapout)
    class(r_type) :: map
    class(r_type) :: mapout
  end subroutine
end module

! { dg-final { cleanup-modules "base_mod r_mod" } }
