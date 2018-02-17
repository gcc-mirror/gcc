! { dg-do run }
!
! PR 45271: [OOP] Polymorphic code breaks when changing order of USE statements
!
! Contributed by Harald Anlauf <anlauf@gmx.de>

module abstract_vector
  implicit none
  type, abstract :: vector_class
  contains
    procedure(op_assign_v_v), deferred :: assign
  end type vector_class
  abstract interface
    subroutine op_assign_v_v(this,v)
      import vector_class
      class(vector_class), intent(inout) :: this
      class(vector_class), intent(in)    :: v
    end subroutine
  end interface
end module abstract_vector

module concrete_vector
  use abstract_vector
  implicit none
  type, extends(vector_class) :: trivial_vector_type
  contains
    procedure :: assign => my_assign
  end type
contains
  subroutine my_assign (this,v)
    class(trivial_vector_type), intent(inout) :: this
    class(vector_class),        intent(in)    :: v
    write (*,*) 'Oops in concrete_vector::my_assign'
    STOP 1
  end subroutine
end module concrete_vector

module concrete_gradient
  use abstract_vector
  implicit none
  type, extends(vector_class) :: trivial_gradient_type
  contains
    procedure :: assign => my_assign
  end type
contains
  subroutine my_assign (this,v)
    class(trivial_gradient_type), intent(inout) :: this
    class(vector_class),          intent(in)    :: v
    write (*,*) 'concrete_gradient::my_assign'
  end subroutine
end module concrete_gradient

program main
  !--- exchange these two lines to make the code work:
  use concrete_vector    ! (1)
  use concrete_gradient  ! (2)
  !---
  implicit none
  type(trivial_gradient_type)      :: g_initial
  class(vector_class),  allocatable :: g
  print *, "cg: before g%assign"
  allocate(trivial_gradient_type :: g)
  call g%assign (g_initial)
  print *, "cg: after  g%assign"
end program main
