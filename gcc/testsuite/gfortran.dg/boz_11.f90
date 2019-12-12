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

   if (cmplx(b'01000000001010010101001111111101',x,4) /= r) STOP 1
   if (cmplx(x,b'01000000001010010101001111111101',4) /= z) STOP 2

end program test0
