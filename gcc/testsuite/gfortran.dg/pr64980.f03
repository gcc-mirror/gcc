! { dg-do compile }

  implicit none

  type :: muli_trapezium_t
     integer::dim=0
  end type

  type, extends (muli_trapezium_t) :: muli_trapezium_node_class_t
  end type

  class(muli_trapezium_node_class_t), pointer :: node
  print *,get_d_value_array(node)

contains

  function get_d_value_array (this) result (subarray)
    class(muli_trapezium_t), intent(in) :: this
    real, dimension(this%dim) :: subarray
  end function

end
