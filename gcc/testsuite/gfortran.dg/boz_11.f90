! { dg-do run }
!
program test0
   implicit none
   real, parameter :: &
     r = transfer(int(b'01000000001010010101001111111101',kind=4),0.)
   complex, parameter :: z = r * (0, 1.)
   real(kind=8), parameter :: rd = dble(b'00000000000000000000000000000000&
                                         &01000000001010010101001111111101')
   complex(kind=8), parameter :: zd = (0._8, 1._8) * rd
   integer :: x = 0

   if (cmplx(b'01000000001010010101001111111101',x,4) /= r) call abort
   if (cmplx(x,b'01000000001010010101001111111101',4) /= z) call abort
   if (complex(b'01000000001010010101001111111101',0) /= r) call abort
   if (complex(0,b'01000000001010010101001111111101') /= z) call abort

   !if (cmplx(b'00000000000000000000000000000000&
   !           &01000000001010010101001111111101',x,8) /= rd) call abort
   !if (cmplx(x,b'00000000000000000000000000000000&
   !             &01000000001010010101001111111101',8) /= zd) call abort
   !if (dcmplx(b'00000000000000000000000000000000&
   !            &01000000001010010101001111111101',x) /= rd) call abort
   !if (dcmplx(x,b'00000000000000000000000000000000&
   !              &01000000001010010101001111111101') /= zd) call abort

end program test0
