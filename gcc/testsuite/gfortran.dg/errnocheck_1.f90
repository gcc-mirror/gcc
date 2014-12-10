! { dg-do compile { target i?86-*-* x86_64-*-* } }
! Fortran should default to -fno-math-errno
! and thus no call to sqrt in asm
subroutine mysqrt(a)
 real(KIND=KIND(0.0D0)) :: a
 a=sqrt(a)
end subroutine
! { dg-final { scan-assembler-times "call" 0 } }
