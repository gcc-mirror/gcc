! { dg-do compile }
!
! PR 48059: [4.6 Regression][OOP] ICE in in gfc_conv_component_ref: character function of extended type
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module a_module
  type :: a_type
     integer::length=0
  end type a_type
  type,extends(a_type) :: b_type
  end type b_type
contains
  function a_string(this) result(form)
    class(a_type),intent(in)::this
    character(max(1,this%length))::form
  end function a_string
  subroutine b_sub(this)
    class(b_type),intent(inout),target::this
    print *,a_string(this)
  end subroutine b_sub
end module a_module

! { dg-final { cleanup-modules "a_module" } }
