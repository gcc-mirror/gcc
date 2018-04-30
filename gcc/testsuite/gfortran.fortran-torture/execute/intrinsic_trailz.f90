program test_intrinsic_trailz
   implicit none

   call test_trailz(0_1,0_2,0_4,0_8,1_1,1_2,1_4,1_8,8_1,8_2,8_4,8_8)
   stop

   contains

        subroutine test_trailz(z1,z2,z4,z8,i1,i2,i4,i8,e1,e2,e4,e8)
           integer(kind=1) :: z1, i1, e1
           integer(kind=2) :: z2, i2, e2
           integer(kind=4) :: z4, i4, e4
           integer(kind=8) :: z8, i8, e8

           if (trailz(0_1) /=  8) STOP 1
           if (trailz(0_2) /= 16) STOP 2
           if (trailz(0_4) /= 32) STOP 3
           if (trailz(0_8) /= 64) STOP 4

           if (trailz(1_1) /=  0) STOP 5
           if (trailz(1_2) /=  0) STOP 6
           if (trailz(1_4) /=  0) STOP 7
           if (trailz(1_8) /=  0) STOP 8

           if (trailz(8_1) /=  3) STOP 9
           if (trailz(8_2) /=  3) STOP 10
           if (trailz(8_4) /=  3) STOP 11
           if (trailz(8_8) /=  3) STOP 12

           if (trailz(z1) /=  8) STOP 13
           if (trailz(z2) /= 16) STOP 14
           if (trailz(z4) /= 32) STOP 15
           if (trailz(z8) /= 64) STOP 16

           if (trailz(i1) /=  0) STOP 17
           if (trailz(i2) /=  0) STOP 18
           if (trailz(i4) /=  0) STOP 19
           if (trailz(i8) /=  0) STOP 20

           if (trailz(e1) /=  3) STOP 21
           if (trailz(e2) /=  3) STOP 22
           if (trailz(e4) /=  3) STOP 23
           if (trailz(e8) /=  3) STOP 24
        end subroutine test_trailz

end program
