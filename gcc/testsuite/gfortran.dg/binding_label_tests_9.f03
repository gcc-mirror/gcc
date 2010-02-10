! { dg-do compile }
module x
  use iso_c_binding
  implicit none
  private :: bar
  private :: my_private_sub
  private :: my_private_sub_2
  public :: my_public_sub
contains
  subroutine bar() bind(c,name="foo") ! { dg-warning "PRIVATE but has been given the binding label" }
  end subroutine bar
  
  subroutine my_private_sub() bind(c, name="")
  end subroutine my_private_sub

  subroutine my_private_sub_2() bind(c) ! { dg-warning "PRIVATE but has been given the binding label" }
  end subroutine my_private_sub_2

  subroutine my_public_sub() bind(c, name="my_sub")
  end subroutine my_public_sub
end module x

! { dg-final { cleanup-modules "x" } }
