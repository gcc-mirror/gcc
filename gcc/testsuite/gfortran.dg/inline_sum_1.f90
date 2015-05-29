! { dg-do compile }
! { dg-options "-Warray-temporaries -O -fdump-tree-original" }
!
! PR fortran/43829
! Scalarization of reductions.
! Test that sum is properly inlined.

! This is the compile time test only; for the runtime test see inline_sum_2.f90
! We can't test for temporaries on the run time test directly, as it tries
! several optimization options among which -Os, and sum inlining is disabled
! at -Os.


  implicit none


  integer :: i, j, k

  integer, parameter :: q = 2
  integer, parameter :: nx=3, ny=2*q, nz=5
  integer, parameter, dimension(nx,ny,nz) :: p  = &
        & reshape ((/ (i**2, i=1,size(p)) /), shape(p))

  integer, parameter, dimension(   ny,nz) :: px = &
        & reshape ((/ (( &
        &        nx*(  nx*j+nx*ny*k+1)*(  nx*j+nx*ny*k+1+      (nx-1)) &
        &       +      nx*(nx-1)*(2*nx-1)/6, &
        &       j=0,ny-1), k=0,nz-1) /), shape(px))

  integer, parameter, dimension(nx,   nz) :: py = &
        & reshape ((/ (( &
        &        ny*(i     +nx*ny*k+1)*(i     +nx*ny*k+1+nx   *(ny-1)) &
        &       +(nx   )**2*ny*(ny-1)*(2*ny-1)/6, &
        &       i=0,nx-1), k=0,nz-1) /), shape(py))

  integer, parameter, dimension(nx,ny   ) :: pz = &
        & reshape ((/ (( &
        &        nz*(i+nx*j        +1)*(i+nx*j        +1+nx*ny*(nz-1)) &
        &       +(nx*ny)**2*nz*(nz-1)*(2*nz-1)/6, &
        &       i=0,nx-1), j=0,ny-1) /), shape(pz))


  integer, dimension(nx,ny,nz) :: a
  integer, dimension(   ny,nz) :: ax
  integer, dimension(nx,   nz) :: ay
  integer, dimension(nx,ny   ) :: az

  logical, dimension(nx,ny,nz) :: m, true


  integer, dimension(nx,ny) :: b

  integer, dimension(nx,nx) :: onesx
  integer, dimension(ny,ny) :: onesy
  integer, dimension(nz,nz) :: onesz


  a    = p
  m    = reshape((/ ((/ .true., .false. /), i=1,size(m)/2) /), shape(m))
  true = reshape((/ (.true., i=1,size(true)) /), shape(true))

  onesx = reshape((/ ((1, j=1,i),(0,j=1,nx-i),i=1,size(onesx,2)) /), shape(onesx))
  onesy = reshape((/ ((1, j=1,i),(0,j=1,ny-i),i=1,size(onesy,2)) /), shape(onesy))
  onesz = reshape((/ ((1, j=1,i),(0,j=1,nz-i),i=1,size(onesz,2)) /), shape(onesz))

  ! Correct results in simple cases
  ax = sum(a,1)
  if (any(ax /= px)) call abort

  ay = sum(a,2)
  if (any(ay /= py)) call abort

  az = sum(a,3)
  if (any(az /= pz)) call abort


  ! Masks work
  if (any(sum(a,1,.false.) /= 0))                    call abort
  if (any(sum(a,2,.true.)  /= py))                   call abort
  if (any(sum(a,3,m)       /= merge(pz,0,m(:,:,1)))) call abort
  if (any(sum(a,2,m)       /= merge(sum(a(:, ::2,:),2),&
                                    sum(a(:,2::2,:),2),&
                                    m(:,1,:))))      call abort


  ! It works too with array constructors ...
  if (any(sum(                                      &
        reshape((/ (i*i,i=1,size(a)) /), shape(a)), &
        1,                                          &
        true) /= ax)) call abort

  ! ... and with vector subscripts
  if (any(sum(               &
        a((/ (i,i=1,nx) /),  &
          (/ (i,i=1,ny) /),  &
          (/ (i,i=1,nz) /)), &
        1) /= ax)) call abort

  if (any(sum(                &
        a(sum(onesx(:,:),1),  & ! unnecessary { dg-warning "Creating array temporary" }
          sum(onesy(:,:),1),  & ! unnecessary { dg-warning "Creating array temporary" }
          sum(onesz(:,:),1)), & ! unnecessary { dg-warning "Creating array temporary" }
        1) /= ax)) call abort


  ! Nested sums work
  if (sum(sum(sum(a,1),1),1) /= sum(a)) call abort
  if (sum(sum(sum(a,1),2),1) /= sum(a)) call abort
  if (sum(sum(sum(a,3),1),1) /= sum(a)) call abort
  if (sum(sum(sum(a,3),2),1) /= sum(a)) call abort

  if (any(sum(sum(a,1),1) /= sum(sum(a,2),1))) call abort
  if (any(sum(sum(a,1),2) /= sum(sum(a,3),1))) call abort
  if (any(sum(sum(a,2),2) /= sum(sum(a,3),2))) call abort


  ! Temps are unavoidable here (function call's argument or result)
  ax = sum(neid3(a),1)          ! { dg-warning "Creating array temporary" }
  ! Sums as part of a bigger expr work
  if (any(1+sum(eid(a),1)+ax+sum( &
        neid3(a), &            ! { dg-warning "Creating array temporary" }
        1)+1  /= 3*ax+2))        call abort
  if (any(1+eid(sum(a,2))+ay+ &
        neid2( &               ! { dg-warning "Creating array temporary" }
        sum(a,2) &             ! { dg-warning "Creating array temporary" }
        )+1  /= 3*ay+2))        call abort
  if (any(sum(eid(sum(a,3))+az+2* &
        neid2(az) &            ! { dg-warning "Creating array temporary" }
        ,1)+1 /= 4*sum(az,1)+1)) call abort

  if (any(sum(transpose(sum(a,1)),1)+sum(az,1) /= sum(ax,2)+sum(sum(a,3),1))) call abort


  ! Creates a temp when needed. 
  a(1,:,:) = sum(a,1)                   ! unnecessary { dg-warning "Creating array temporary" }
  if (any(a(1,:,:) /= ax)) call abort

  b = p(:,:,1)
  call set(b(2:,1), sum(b(:nx-1,:),2))  ! { dg-warning "Creating array temporary" }
  if (any(b(2:,1) /= ay(1:nx-1,1))) call abort

  b = p(:,:,1)
  call set(b(:,1), sum(b,2))            ! unnecessary { dg-warning "Creating array temporary" }
  if (any(b(:,1) /= ay(:,1))) call abort

  b = p(:,:,1)
  call tes(sum(eid(b(:nx-1,:)),2), b(2:,1))  ! { dg-warning "Creating array temporary" }
  if (any(b(2:,1) /= ay(1:nx-1,1))) call abort

  b = p(:,:,1)
  call tes(eid(sum(b,2)), b(:,1))            ! unnecessary { dg-warning "Creating array temporary" }
  if (any(b(:,1) /= ay(:,1))) call abort

contains

  elemental function eid (x)
    integer, intent(in) :: x
    integer             :: eid

    eid = x
  end function eid

  function neid2 (x)
    integer, intent(in) :: x(:,:)
    integer             :: neid2(size(x,1),size(x,2))

    neid2 = x
  end function neid2

  function neid3 (x)
    integer, intent(in) :: x(:,:,:)
    integer             :: neid3(size(x,1),size(x,2),size(x,3))

    neid3 = x
  end function neid3

  elemental subroutine set (o, i)
    integer, intent(in)  :: i
    integer, intent(out) :: o

    o = i
  end subroutine set

  elemental subroutine tes (i, o)
    integer, intent(in)  :: i
    integer, intent(out) :: o

    o = i
  end subroutine tes
end
! { dg-final { scan-tree-dump-times "struct array._integer\\(kind=4\\) atmp" 13 "original" } }
! { dg-final { scan-tree-dump-times "struct array\[^\\n\]*atmp" 13 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_sum_" 0 "original" } }
