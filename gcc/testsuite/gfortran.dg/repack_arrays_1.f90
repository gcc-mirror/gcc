! { dg-do run }
! { dg-options "-frepack-arrays" }
!
! Check that arrays marked with TARGET attribute are not repacked.
!
program test2
   use iso_c_binding
   implicit none
   real, target :: x(7)
   type(c_ptr) cp1, cp2

   x = 42
   if (.not. c_associated(c_loc(x(3)),point(x(::2)))) STOP 1
contains
  function point(x)
    use iso_c_binding
    real, intent(in), target :: x(:)
    type(c_ptr) point
    real, pointer :: p

    p => x(2)
    point = c_loc(p)
  end function point
end program test2 
