! { dg-do compile }
! { dg-options "-O2 -ftree-loop-distribution" }

MODULE NFT_mod

implicit none
integer :: Nangle
real:: Z0
real, dimension(:,:), allocatable :: Angle
real, dimension(:), allocatable :: exth, ezth, hxth, hyth, hyphi

CONTAINS

SUBROUTINE NFT_Init()

real :: th, fi
integer :: n

do n = 1,Nangle
  th = Angle(n,1)
  fi = Angle(n,2)

  exth(n) =  cos(fi)*cos(th)
  ezth(n) = -sin(th)
  hxth(n) = -sin(fi)
  hyth(n) =  cos(fi)
  hyphi(n) = -sin(fi)
end do
END SUBROUTINE NFT_Init

END MODULE NFT_mod

! { dg-final { cleanup-modules "nft_mod" } }
