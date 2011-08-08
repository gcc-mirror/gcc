! { dg-do run }
!
! PR 49562: [4.6/4.7 Regression] [OOP] assigning value to type-bound function
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module ice
  type::ice_type
   contains
     procedure::ice_func
  end type
  integer, target :: it = 0
contains
  function ice_func(this)
    integer, pointer :: ice_func
    class(ice_type)::this
    ice_func => it
  end function ice_func
  subroutine ice_sub(a)
    class(ice_type)::a
    a%ice_func() = 1
  end subroutine ice_sub
end module

use ice
type(ice_type) :: t
if (it/=0) call abort()
call ice_sub(t)
if (it/=1) call abort()
end

! { dg-final { cleanup-modules "ice" } }
