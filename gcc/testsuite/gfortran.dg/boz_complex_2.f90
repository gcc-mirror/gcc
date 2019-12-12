! { dg-do compile }
! { dg-options "-fallow-invalid-boz" }
program foo

   implicit none

   complex(4) z

   z = complex(z'4444', 42)         ! { dg-warning "cannot appear in the" }
   if (real(z,4) /= 17476.0 .or. aimag(z) /= 42.0) stop 2

   z = complex(z'44444400', 42.)    ! { dg-warning "cannot appear in the" }
   if (real(z,4) /= 785.062500 .or. aimag(z) /= 42.0) stop 3

end program foo
