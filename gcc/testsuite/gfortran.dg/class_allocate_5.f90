! { dg-do run }
!
! PR fortran/45451
!
! Contributed by Salvatore Filippone and Janus Weil
!
! Check that ALLOCATE with SOURCE= does a deep copy.
!
program bug23
  implicit none

  type  :: psb_base_sparse_mat
    integer, allocatable :: irp(:)
  end type psb_base_sparse_mat

  class(psb_base_sparse_mat), allocatable  :: a 
  type(psb_base_sparse_mat) :: acsr

  allocate(acsr%irp(4)) 
  acsr%irp(1:4) = (/1,3,4,5/)

  write(*,*) acsr%irp(:)

  allocate(a,source=acsr)

  write(*,*) a%irp(:)

  call move_alloc(acsr%irp, a%irp)

  write(*,*) a%irp(:)

  if (any (a%irp /= [1,3,4,5])) call abort()
end program bug23

