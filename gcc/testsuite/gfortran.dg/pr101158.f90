! { dg-do compile }
! { dg-options "-O1 -ftree-slp-vectorize -fwrapv" }
! { dg-additional-options "-march=armv8-a+sve" { target aarch64-*-* } }

subroutine sprpl5 (left)
  implicit none

  integer :: left
  integer :: avail1, avail2, delx1, delx2, i2, ic

  ic = left
  delx1 = ic / 2
  delx2 = delx1 + 1
  i2 = ic + delx2
  avail1 = i2
  avail2 = 1

  do delx1 = 1, 2
     ic = left + nint (real (left) / 2)
     if (ic .ge. avail1) avail1 = ic + 1

     i2 = ic + delx2
     if (i2 .le. avail2) avail2 = i2 + 1
  end do
end subroutine sprpl5
