! { dg-do compile }
! { dg-additional-options "-march=armv8.3-a" { target aarch64-*-* } }
subroutine cunhj (tfn, asum, bsum)
  implicit none
  complex :: up, tfn, asum, bsum
  real :: ar

  up = tfn * ar
  bsum = up + ar
  asum = up + asum
  return
end subroutine cunhj
