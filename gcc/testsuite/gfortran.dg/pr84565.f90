! PR fortran/84565
! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-mlow-precision-sqrt -funsafe-math-optimizations" }
subroutine mysqrt(a)
 real(KIND=KIND(0.0D0)) :: a
 a=sqrt(a)
end subroutine
