! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-require-effective-target ilp32 }
! { dg-options "-O -msse -mfpmath=sse" }
! { dg-require-effective-target sse }
    subroutine yhalf(z)
    complex cdexpj,z
    z=cdexpj((0.d0,1.d0)*z)
    end
