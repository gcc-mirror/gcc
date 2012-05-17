! { dg-do compile }
!
! PR 44211: [OOP] ICE with TBP of pointer component of derived type array
!
! Original test case by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>
! Modified by Janus Weil <janus@gcc.gnu.org>

module ice_module
  type::ice_type
     class(ice_type),pointer::next
   contains
     procedure::ice_sub
     procedure::ice_fun
  end type ice_type
contains
  subroutine ice_sub(this)
    class(ice_type)::this
  end subroutine
  integer function ice_fun(this)
    class(ice_type)::this
  end function
  subroutine ice()
    type(ice_type),dimension(2)::ice_array
    call ice_array(1)%next%ice_sub()
    print *,ice_array(2)%next%ice_fun()
  end subroutine
end module ice_module
