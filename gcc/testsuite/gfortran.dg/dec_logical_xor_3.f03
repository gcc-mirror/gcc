! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Test errors for logical .XOR. operator with a real standard.
!

implicit none

logical, volatile :: in1, in2, xor_out
xor_out = in1 .xor. in2 ! { dg-error ".XOR. operator" }

end
