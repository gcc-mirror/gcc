! { dg-do compile }

  integer, parameter :: q = 2
  integer, parameter :: nx=3, ny=2*q, nz=5
  integer, parameter, dimension(nx,ny,nz) :: p  = &
        & reshape ((/ (i**2, i=1,size(p)) /), shape(p))
  integer, parameter, dimension(   ny,nz) :: px = &
        & reshape ((/ (( &
        &       +      nx*(nx-1)*(2*nx-1)/6, &
        &       j=0,ny-1), k=0,nz-1) /), shape(px))
  integer, parameter, dimension(nx,   nz) :: py = &
        & reshape ((/ (( &
        &       +(nx   )**2*ny*(ny-1)*(2*ny-1)/6, &
        &       i=0,nx-1), k=0,nz-1) /), shape(py))
  integer, parameter, dimension(nx,ny   ) :: pz = &
        & reshape ((/ (( &
        &       +(nx*ny)**2*nz*(nz-1)*(2*nz-1)/6, &
        &       i=0,nx-1), j=0,ny-1) /), shape(pz))
  integer, dimension(nx,ny,nz) :: a
  integer, dimension(nx,ny   ) :: az
  if (sum(sum(sum(a,1),2),1) /= sum(a)) STOP 1
  if (sum(sum(sum(a,3),1),1) /= sum(a)) STOP 2
  if (any(1+sum(eid(a),1)+ax+sum( &
        neid3(a), &
        1)+1  /= 3*ax+2))        STOP 3
  if (any(1+eid(sum(a,2))+ay+ &
        neid2( &
        sum(a,2) &
        )+1  /= 3*ay+2))        STOP 4
  if (any(sum(eid(sum(a,3))+az+2* &
        neid2(az) &
        ,1)+1 /= 4*sum(az,1)+1)) STOP 5
contains
  elemental function eid (x)
    integer, intent(in) :: x
  end function eid
  function neid2 (x)
    integer, intent(in) :: x(:,:)
    integer             :: neid2(size(x,1),size(x,2))
    neid2 = x
  end function neid2
  function neid3 (x)
    integer, intent(in) :: x(:,:,:)
    integer             :: neid3(size(x,1),size(x,2),size(x,3))
  end function neid3
  elemental subroutine set (o, i)
    integer, intent(in)  :: i
    integer, intent(out) :: o
  end subroutine set
  elemental subroutine tes (i, o)
    integer, intent(in)  :: i
    integer, intent(out) :: o
  end subroutine tes
end


