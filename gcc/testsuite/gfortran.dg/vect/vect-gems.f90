! { dg-do compile }
! { dg-require-effective-target vect_double }

MODULE UPML_mod

IMPLICIT NONE

!PUBLIC UPMLupdateE
!
!PRIVATE

real(kind=8), dimension(:,:,:), allocatable :: Dx_ilow

real(kind=8), dimension(:), allocatable :: aye, aze
real(kind=8), dimension(:), allocatable :: bye, bze
real(kind=8), dimension(:), allocatable :: fxh, cxh

real(kind=8) :: epsinv
real(kind=8) :: dxinv, dyinv, dzinv

integer :: xstart, ystart, zstart, xstop, ystop, zstop

CONTAINS

SUBROUTINE UPMLupdateE(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)

integer, intent(in) :: nx, ny, nz
real(kind=8), intent(inout),                                                &
         dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
real(kind=8), intent(inout),                                                &
         allocatable :: Hx(:,:,:), Hy(:,:,:), Hz(:,:,:)

integer :: i, j, k
real(kind=8) :: Dxold, Dyold, Dzold

do k=zstart+1,zstop
  do j=ystart+1,ystop
    do i=xstart+1,0

      Dxold = Dx_ilow(i,j,k)

      Dx_ilow(i,j,k) = aye(j) * Dx_ilow(i,j,k) +                              &
                       bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +            &
                                 (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_ilow(i,j,k) - fxh(i)*Dxold) * epsinv
    end do
  end do
end do

END SUBROUTINE UPMLupdateE

END MODULE UPML_mod

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } }
