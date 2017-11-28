! { dg-do run }
!
! Tests the fix for PR81758, in which the vpointer for 'ptr' in
! function 'pointer_value' would be set to the vtable of the component
! 'container' rather than that of the component 'vec_elem'. In this test
! case it is ensured that there is a single typebound procedure for both
! types, so that different values are returned. In the original problem
! completely different procedures were involved so that a segfault resulted.
!
! Reduced from the original code of Dimitry Liakh  <liakhdi@ornl.gov> by
!                                   Paul Thomas  <pault@gcc.gnu.org>
!
module types
  type, public:: gfc_container_t
  contains
    procedure, public:: get_value => ContTypeGetValue
  end type gfc_container_t

  !Element of a container:
  type, public:: gfc_cont_elem_t
    integer :: value_p
  contains
    procedure, public:: get_value => ContElemGetValue
  end type gfc_cont_elem_t

  !Vector element:
  type, extends(gfc_cont_elem_t), public:: vector_elem_t
  end type vector_elem_t

  !Vector:
  type, extends(gfc_container_t), public:: vector_t
    type(vector_elem_t), allocatable, private :: vec_elem
  end type vector_t

  type, public :: vector_iter_t
    class(vector_t), pointer, private :: container => NULL()
  contains
    procedure, public:: get_vector_value => vector_Value
    procedure, public:: get_pointer_value => pointer_value
  end type

contains
  integer function ContElemGetValue (this)
    class(gfc_cont_elem_t) :: this
    ContElemGetValue = this%value_p
  end function

  integer function ContTypeGetValue (this)
    class(gfc_container_t) :: this
    ContTypeGetValue = 0
  end function

  integer function vector_Value (this)
    class(vector_iter_t) :: this
    vector_value = this%container%vec_elem%get_value()
  end function

  integer function pointer_value (this)
    class(vector_iter_t), target :: this
    class(gfc_cont_elem_t), pointer :: ptr
    ptr => this%container%vec_elem
    pointer_value = ptr%get_value()
  end function

  subroutine factory (arg)
    class (vector_iter_t), pointer :: arg
    allocate (vector_iter_t :: arg)
    allocate (vector_t :: arg%container)
    allocate (arg%container%vec_elem)
    arg%container%vec_elem%value_p = 99
  end subroutine
end module

  use types
  class (vector_iter_t), pointer :: x

  call factory (x)
  if (x%get_vector_value() .ne. 99) call abort
  if (x%get_pointer_value() .ne. 99) call abort
end
