! { dg-do run }
! { dg-additional-sources iso_c_binding_rename_2_driver.c }
module mod0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_associated
end module mod0

module mod1
  use mod0, my_c_ptr => c_ptr, my_c_associated => c_associated
end module mod1

module mod2
contains
  subroutine sub2(my_ptr1) bind(c)
    use mod1, my_c_ptr_2 => my_c_ptr, my_c_associated_2 => my_c_associated
    implicit none
    type(my_c_ptr_2) :: my_ptr1
    if( .not. my_c_associated_2(my_ptr1)) then
       STOP 1
    end if
  end subroutine sub2

  subroutine sub3(my_ptr1) bind(c)
    use mod1, my_c_ptr_2 => my_c_ptr
    implicit none
    type(my_c_ptr_2) :: my_ptr1
    if( .not. my_c_associated(my_ptr1)) then
       STOP 2
    end if
  end subroutine sub3

  subroutine sub4(my_ptr1) bind(c)
    use mod1, my_c_associated_3 => my_c_associated
    implicit none
    type(my_c_ptr) :: my_ptr1
    if( .not. my_c_associated_3(my_ptr1)) then
       STOP 3
    end if
  end subroutine sub4

end module mod2
