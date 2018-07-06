! { dg-do compile }
module c_assoc_2
  use, intrinsic :: iso_c_binding, only: c_ptr, c_associated

contains
  subroutine sub0(my_c_ptr) bind(c)
    type(c_ptr), value :: my_c_ptr
    type(c_ptr), pointer :: my_c_ptr_2
    integer :: my_integer
    
    if(.not. c_associated(my_c_ptr)) then
       STOP 1
    end if
    
    if(.not. c_associated(my_c_ptr, my_c_ptr)) then
       STOP 2
    end if

    if(.not. c_associated(my_c_ptr, my_c_ptr, my_c_ptr)) then ! { dg-error "Too many arguments in call" }
       STOP 3
    end if

    if(.not. c_associated()) then ! { dg-error "Missing actual argument" }
       STOP 4
    end if

    if(.not. c_associated(my_c_ptr_2)) then
       STOP 5
    end if

    if(.not. c_associated(my_integer)) then ! { dg-error "shall have the type TYPE.C_PTR. or TYPE.C_FUNPTR." }
       STOP 6
    end if
  end subroutine sub0

end module c_assoc_2
