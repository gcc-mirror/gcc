! { dg-do compile }
module c_assoc_2
  use, intrinsic :: iso_c_binding, only: c_ptr, c_associated

contains
  subroutine sub0(my_c_ptr) bind(c)
    type(c_ptr), value :: my_c_ptr
    type(c_ptr), pointer :: my_c_ptr_2
    integer :: my_integer
    
    if(.not. c_associated(my_c_ptr)) then
       call abort()
    end if
    
    if(.not. c_associated(my_c_ptr, my_c_ptr)) then
       call abort()
    end if

    if(.not. c_associated(my_c_ptr, my_c_ptr, my_c_ptr)) then ! { dg-error "Too many arguments in call" }
       call abort()
    end if

    if(.not. c_associated()) then ! { dg-error "Missing actual argument 'C_PTR_1' in call to 'c_associated'" }
       call abort()
    end if

    if(.not. c_associated(my_c_ptr_2)) then
       call abort()
    end if

    if(.not. c_associated(my_integer)) then ! { dg-error "shall have the type TYPE.C_PTR. or TYPE.C_FUNPTR." }
       call abort()
    end if
  end subroutine sub0

end module c_assoc_2
