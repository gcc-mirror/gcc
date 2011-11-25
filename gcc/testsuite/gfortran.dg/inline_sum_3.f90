! { dg-do run }
!
! PR fortran/51250
! Wrong loop shape for SUM when arguments are library-allocated arrays.
!
! Original testcase provided by Harald Anlauf <anlauf@gmx.de>

program gfcbug115
  implicit none
  integer :: n_obstype = 2
  integer :: nboxes = 1
  integer :: nprocs = 1
  integer :: nbox, j
  integer, allocatable :: nbx(:,:), pes(:)

  allocate (pes(nboxes))
  allocate (nbx(n_obstype,nboxes))
  nbx(:,:) = 1
  do j = 1, nboxes
     pes(j) = modulo (j-1, nprocs)
  end do
  if (any(nbx /= 1)) call abort
  do j = 0, nprocs-1
     if (.not. all(spread (pes==j,dim=1,ncopies=n_obstype))) call abort
     ! The two following tests used to fail
     if (any(shape(sum(nbx,dim=2,mask=spread (pes==j,dim=1,ncopies=n_obstype))) &
             /= (/ 2 /))) call abort
     if (any(sum (nbx,dim=2,mask=spread (pes==j,dim=1,ncopies=n_obstype)) &
             /= (/ 1, 1 /))) call abort
  end do
end program gfcbug115
