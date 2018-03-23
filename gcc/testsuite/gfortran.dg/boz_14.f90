! { dg-do run }
! { dg-add-options ieee }

! PR fortran/36214
! For BOZ-initialization of floats, the precision used to be wrong sometimes.

   implicit none
   real(4) r
   real(8) rd
   complex(8) z
   rd = &
    real (b'00000000000000000000000000000000&
           &01000000001010010101001111111101',8)
   z  = &
    cmplx(b'00000000000000000000000000000000&
           &01000000001010010101001111111101',0,8)
   r = 0.
   if (z /= rd) STOP 1
   end
