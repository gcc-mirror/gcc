! { dg-do compile }
!
! PR 42353: [OOP] Bogus Error: Name 'vtype$...' at (1) is an ambiguous reference ...
!
! Original test case by Harald Anlauf <anlauf@gmx.de>
! Modified by Janus Weil <janus@gcc.gnu.org>

module concrete_vector
  type :: trivial_vector_type
  end type
  class(trivial_vector_type), pointer :: this
end module concrete_vector

module concrete_gradient
contains
  subroutine my_to_vector (v)
    use concrete_vector
    class(trivial_vector_type) :: v
    select type (v)
    class is (trivial_vector_type)
    end select
  end subroutine
end module concrete_gradient

module concrete_inner_product
  use concrete_vector
  use concrete_gradient
contains
  real function my_dot_v_v (a)
    class(trivial_vector_type) :: a
    select type (a)
    class is (trivial_vector_type)
    end select
  end function
end module concrete_inner_product
 
! { dg-final { cleanup-modules "concrete_vector concrete_gradient concrete_inner_product" } }
