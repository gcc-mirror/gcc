program test_intrinsic_leadz
   implicit none

   call test_leadz(0_1,0_2,0_4,0_8,1_1,1_2,1_4,1_8,8_1,8_2,8_4,8_8)
   stop

   contains

        subroutine test_leadz(z1,z2,z4,z8,i1,i2,i4,i8,e1,e2,e4,e8)
           integer(kind=1) :: z1, i1, e1
           integer(kind=2) :: z2, i2, e2
           integer(kind=4) :: z4, i4, e4
           integer(kind=8) :: z8, i8, e8

           if (leadz(0_1) /=  8) call abort()
           if (leadz(0_2) /= 16) call abort()
           if (leadz(0_4) /= 32) call abort()
           if (leadz(0_8) /= 64) call abort()

           if (leadz(1_1) /=  7) call abort()
           if (leadz(1_2) /= 15) call abort()
           if (leadz(1_4) /= 31) call abort()
           if (leadz(1_8) /= 63) call abort()

           if (leadz(8_1) /=  4) call abort()
           if (leadz(8_2) /= 12) call abort()
           if (leadz(8_4) /= 28) call abort()
           if (leadz(8_8) /= 60) call abort()

           if (leadz(z1) /=  8) call abort()
           if (leadz(z2) /= 16) call abort()
           if (leadz(z4) /= 32) call abort()
           if (leadz(z8) /= 64) call abort()

           if (leadz(i1) /=  7) call abort()
           if (leadz(i2) /= 15) call abort()
           if (leadz(i4) /= 31) call abort()
           if (leadz(i8) /= 63) call abort()

           if (leadz(e1) /=  4) call abort()
           if (leadz(e2) /= 12) call abort()
           if (leadz(e4) /= 28) call abort()
           if (leadz(e8) /= 60) call abort()
        end subroutine test_leadz

end program
