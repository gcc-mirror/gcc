! { dg-do "compile" }
! Test the final fix for PR42353, in which a compilation error was
! occurring because the derived type of the initializer of the vtab
! component '$extends' was not the same as that of the component.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
module abstract_vector
  implicit none

  type, abstract :: vector_class
  end type vector_class
end module abstract_vector
!-------------------------
module concrete_vector
  use abstract_vector
  implicit none

  type, extends(vector_class) :: trivial_vector_type
  end type trivial_vector_type

  private :: my_assign
contains
  subroutine my_assign (this,v)
    class(trivial_vector_type), intent(inout) :: this
    class(vector_class),        intent(in)    :: v
  end subroutine my_assign
end module concrete_vector
!---------------------------
module concrete_gradient
  use abstract_vector
  implicit none

  type, abstract, extends(vector_class) :: gradient_class
  end type gradient_class

  type, extends(gradient_class) :: trivial_gradient_type
  end type trivial_gradient_type

  private :: my_assign
contains
  subroutine my_assign (this,v)
    class(trivial_gradient_type), intent(inout) :: this
    class(vector_class),          intent(in)    :: v
  end subroutine my_assign
end module concrete_gradient
!----------------------------
module concrete_inner_product
  use concrete_vector
  use concrete_gradient
  implicit none
end module concrete_inner_product
! { dg-final { cleanup-modules "abstract_vector concrete_vector" } }
! { dg-final { cleanup-modules "concrete_gradient concrete_inner_product" } }
