! { dg-do run }
program main
   use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
   implicit none
   integer, dimension(2,1,2), target :: table
   table = reshape ( (/ 1,2,-1,-2/), (/2,1,2/))
   call set_table (c_loc (table))
contains
   subroutine set_table (cptr)
     type(c_ptr), intent(in) :: cptr
     integer, dimension(:,:,:), pointer :: table_tmp
     call c_f_pointer (cptr, table_tmp, (/2,1,2/))
     if (any(table_tmp /= table)) STOP 1
   end subroutine set_table
end program main
