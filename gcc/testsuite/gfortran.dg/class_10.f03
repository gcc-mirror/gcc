! { dg-do compile }
!
! PR 41800: [OOP] ICE in fold_convert_loc, at fold-const.c:2789
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

module abstract_gradient

  implicit none
  private

  type, public, abstract :: gradient_class
  contains
    procedure, nopass  :: inner_product
  end type

contains

  function inner_product ()
    class(gradient_class), pointer :: inner_product
    inner_product => NULL()
  end function

end module


 use abstract_gradient
 class(gradient_class), pointer    :: g_initial, ip_save
 ip_save => g_initial%inner_product()   ! ICE
end

! { dg-final { cleanup-modules "abstract_gradient" } }
