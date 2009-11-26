! { dg-do compile }
!
! PR 42167: [OOP] SELECT TYPE with function return value
!
! Contributed by Damian Rouson <damian@rouson.net>

module bar_module

  implicit none
  type :: bar
    real ,dimension(:) ,allocatable :: f
  contains
    procedure :: total
  end type

contains

  function total(lhs,rhs)
    class(bar) ,intent(in) :: lhs
    class(bar) ,intent(in) :: rhs
    class(bar) ,pointer :: total
    select type(rhs)
      type is (bar)
        allocate(bar :: total)
        select type(total)
          type is (bar)
            total%f = lhs%f + rhs%f
        end select
    end select
  end function

end module 

! { dg-final { cleanup-modules "bar_module" } }
