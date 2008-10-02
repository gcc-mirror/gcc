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

           if (trailz(0_1) /=  8) call abort()
           if (trailz(0_2) /= 16) call abort()
           if (trailz(0_4) /= 32) call abort()
           if (trailz(0_8) /= 64) call abort()

           if (trailz(1_1) /=  0) call abort()
           if (trailz(1_2) /=  0) call abort()
           if (trailz(1_4) /=  0) call abort()
           if (trailz(1_8) /=  0) call abort()

           if (trailz(8_1) /=  3) call abort()
           if (trailz(8_2) /=  3) call abort()
           if (trailz(8_4) /=  3) call abort()
           if (trailz(8_8) /=  3) call abort()

           if (trailz(z1) /=  8) call abort()
           if (trailz(z2) /= 16) call abort()
           if (trailz(z4) /= 32) call abort()
           if (trailz(z8) /= 64) call abort()

           if (trailz(i1) /=  0) call abort()
           if (trailz(i2) /=  0) call abort()
           if (trailz(i4) /=  0) call abort()
           if (trailz(i8) /=  0) call abort()

           if (trailz(e1) /=  3) call abort()
           if (trailz(e2) /=  3) call abort()
           if (trailz(e4) /=  3) call abort()
           if (trailz(e8) /=  3) call abort()
        end subroutine test_trailz

end program
