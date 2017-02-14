! { dg-do compile }
! { dg-options "-std=gnu" }
!
! Test warnings for logical .XOR. operator without -std=legacy.
!

implicit none

logical, volatile :: in1, in2, xor_out
xor_out = in1 .xor. in2 ! { dg-warning ".XOR. operator" }

end
