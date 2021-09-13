! { dg-do run }
! { dg-additional-sources iso_fortran_binding_uint8_array_driver.c }

module m
   use iso_c_binding
contains
   subroutine fsub( x ) bind(C, name="fsub")
      integer(c_int8_t), intent(inout) :: x(:)
      x = x+1
   end subroutine
end module
