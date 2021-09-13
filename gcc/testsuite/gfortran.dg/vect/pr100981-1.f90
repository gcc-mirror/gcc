! { dg-do compile }
! { dg-additional-options "-O3 -ftree-parallelize-loops=2 -fno-signed-zeros -fno-trapping-math" }
! { dg-additional-options "-march=armv8.3-a" { target aarch64*-*-* } }

complex function cdcdot(n, cx)
  implicit none

  integer :: n, i, kx
  complex :: cx(*)
  double precision :: dsdotr, dsdoti, dt1, dt3

  kx = 1
  do i = 1, n
     dt1 = real(cx(kx))
     dt3 = aimag(cx(kx))
     dsdotr = dsdotr + dt1 * 2 - dt3 * 2
     dsdoti = dsdoti + dt1 * 2 + dt3 * 2
     kx = kx + 1
  end do
  cdcdot = cmplx(real(dsdotr), real(dsdoti))
  return
end function cdcdot
