! { dg-do compile }
!
! PR fortran/38894
!
!

subroutine test2
use iso_c_binding
type(c_funptr) :: fun
type(c_ptr) :: fptr
procedure(), pointer :: bar
integer, pointer :: bari
call c_f_procpointer(fptr,bar) ! { dg-error "Argument CPTR at .1. to C_F_PROCPOINTER shall have the type TYPE.C_FUNPTR." }
call c_f_pointer(fun,bari) ! { dg-error "Argument CPTR at .1. to C_F_POINTER shall have the type TYPE.C_PTR." }
fun = fptr ! { dg-error "Can't convert TYPE.c_ptr. to TYPE.c_funptr." }
end

subroutine test()
use iso_c_binding, c_ptr2 => c_ptr
type(c_ptr2) :: fun
procedure(), pointer :: bar
integer, pointer :: foo
call c_f_procpointer(fun,bar) ! { dg-error "Argument CPTR at .1. to C_F_PROCPOINTER shall have the type TYPE.C_FUNPTR." }
call c_f_pointer(fun,foo)  ! OK
end

module rename
  use, intrinsic :: iso_c_binding, only: my_c_ptr_0 => c_ptr
end module rename

program p
  use, intrinsic :: iso_c_binding, my_c_ptr => c_ptr
  type(my_c_ptr) :: my_ptr
  print *,c_associated(my_ptr)
contains
  subroutine sub()
    use rename   ! (***)
    type(my_c_ptr_0) :: my_ptr2
    type(c_funptr) :: myfun
    print *,c_associated(my_ptr,my_ptr2)
    print *,c_associated(my_ptr,myfun) ! { dg-error "Argument C_PTR_2 at .1. to C_ASSOCIATED shall have the same type as C_PTR_1: TYPE.c_ptr. instead of TYPE.c_funptr." }
  end subroutine
end
