! { dg-do run }
! { dg-additional-sources iso_c_binding_rename_1_driver.c }
module iso_c_binding_rename_0
  use, intrinsic :: iso_c_binding, only: my_c_ptr_0 => c_ptr, &
       c_associated
end module iso_c_binding_rename_0


module iso_c_binding_rename_1
  ! rename a couple of the symbols from iso_c_binding.  the compiler 
  ! needs to be able to recognize the derived types with names different
  ! from the one in iso_c_binding because it will look up the derived types
  ! to define the args and return values of some of the procedures in 
  ! iso_c_binding.  this should verify that this functionality works.
  use, intrinsic :: iso_c_binding, my_c_int => c_int, my_c_ptr => c_ptr, &
       my_c_associated => c_associated, my_c_f_pointer => c_f_pointer

contains
  subroutine sub0(my_int) bind(c)
    integer(my_c_int), value :: my_int
    if(my_int .ne. 1) then
       call abort()
    end if
  end subroutine sub0

  subroutine sub1(my_ptr) bind(c)
    type(my_c_ptr), value :: my_ptr

    if(.not. my_c_associated(my_ptr)) then
       call abort()
    end if
  end subroutine sub1

  subroutine sub2(my_int, my_long) bind(c)
    use, intrinsic :: iso_c_binding, my_c_int_2 => c_int, &
         my_c_long_2 => c_long
    integer(my_c_int_2), value :: my_int
    integer(my_c_long_2), value :: my_long

    if(my_int .ne. 1) then
       call abort()
    end if
    if(my_long .ne. 1) then
       call abort()
    end if
  end subroutine sub2

  subroutine sub3(cptr1, cptr2) bind(c)
    type(my_c_ptr), value :: cptr1
    type(my_c_ptr), value :: cptr2
    integer(my_c_int), pointer :: my_f90_c_ptr

    if(.not. my_c_associated(cptr1)) then
       call abort()
    end if

    if(.not. my_c_associated(cptr1, cptr2)) then
       call abort()
    end if

    call my_c_f_pointer(cptr1, my_f90_c_ptr)
  end subroutine sub3

  subroutine sub4(cptr1, cptr2) bind(c)
    ! rename the my_c_ptr_0 from iso_c_binding_rename_0 just to further test
    ! both are actually aliases to c_ptr
    use iso_c_binding_rename_0, my_c_ptr_local => my_c_ptr_0, &
         my_c_associated_2 => c_associated

    implicit none
    type(my_c_ptr_local), value :: cptr1
    type(my_c_ptr_local), value :: cptr2

    if(.not. my_c_associated_2(cptr1)) then
       call abort()
    end if
    
    if(.not. my_c_associated_2(cptr2)) then
       call abort()
    end if
  end subroutine sub4
end module iso_c_binding_rename_1

! { dg-final { cleanup-modules "iso_c_binding_rename_0 iso_c_binding_rename_1" } }
