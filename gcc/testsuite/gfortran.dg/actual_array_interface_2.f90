! { dg-do compile }
program gprogram
   implicit none
   real, dimension(-2:0) :: my_arr
   call fill_array(my_arr)
   contains
      subroutine  fill_array(arr)
         implicit none
         real, dimension(-2:0), intent(out) :: arr
         arr = 42
      end subroutine fill_array
end program gprogram

