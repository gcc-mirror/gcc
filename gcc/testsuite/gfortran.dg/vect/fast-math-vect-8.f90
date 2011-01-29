! { dg-do compile } 
! { dg-require-effective-target vect_float } 

module solv_cap

  implicit none

  public  :: init_solve

  integer, parameter, public :: dp = 4

  real(kind=dp), private :: Pi, Mu0, c0, eps0
  logical,       private :: UseFFT, UsePreco
  real(kind=dp), private :: D1, D2
  integer,       private, save :: Ng1=0, Ng2=0
  integer,       private, pointer,     dimension(:,:)  :: Grid
  real(kind=dp), private, allocatable, dimension(:,:)  :: G

contains

  subroutine init_solve(Grid_in, GrSize1, GrSize2, UseFFT_in, UsePreco_in)
    integer, intent(in), target, dimension(:,:) :: Grid_in
    real(kind=dp), intent(in)  :: GrSize1, GrSize2
    logical,       intent(in)  :: UseFFT_in, UsePreco_in
    integer                    :: i, j

    Pi = acos(-1.0_dp)
    Mu0 = 4e-7_dp * Pi
    c0 = 299792458
    eps0 = 1 / (Mu0 * c0**2)

    UseFFT = UseFFT_in
    UsePreco = UsePreco_in

    if(Ng1 /= 0 .and. allocated(G) ) then
      deallocate( G )
    end if

    Grid => Grid_in
    Ng1 = size(Grid, 1)
    Ng2 = size(Grid, 2)
    D1 = GrSize1/Ng1
    D2 = GrSize2/Ng2

    allocate( G(0:Ng1,0:Ng2) )

    write(unit=*, fmt=*) "Calculating G"
    do i=0,Ng1
      do j=0,Ng2
        G(j,i) = Ginteg( -D1/2,-D2/2, D1/2,D2/2, i*D1,j*D2 )
      end do
    end do

    if(UseFFT) then
      write(unit=*, fmt=*) "Transforming G"
      call FourirG(G,1)
    end if

    return


  contains
  function Ginteg(xq1,yq1, xq2,yq2, xp,yp)  result(G)
    real(kind=dp), intent(in) :: xq1,yq1, xq2,yq2, xp,yp
    real(kind=dp)             :: G
    real(kind=dp)             :: x1,x2,y1,y2,t
    x1 = xq1-xp
    x2 = xq2-xp
    y1 = yq1-yp
    y2 = yq2-yp

    if (x1+x2 < 0) then
      t = -x1
      x1 = -x2
      x2 = t
    end if
    if (y1+y2 < 0) then
      t = -y1
      y1 = -y2
      y2 = t
    end if

    G = (x2*y2)-(x1*y2)-(x2*y1)+(x1*y1)

    return
  end function Ginteg

  end subroutine init_solve

end module solv_cap


! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_intfloat_cvt } } }
! { dg-final { cleanup-tree-dump "vect" } } 
