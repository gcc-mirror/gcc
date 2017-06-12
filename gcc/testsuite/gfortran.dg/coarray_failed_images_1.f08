! { dg-do run }
! { dg-options "-fdump-tree-original -fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program test_failed_images_1
  implicit none

  integer :: me,np,stat
  character(len=1) :: c
  integer, allocatable :: fi(:)
  integer(kind=1), allocatable :: sfi(:)

  fi = failed_images()
  if (size(fi) > 0) error stop "failed_images result shall be empty array"
  if (allocated(fi)) error stop "failed_images result shall not be allocated"

  sfi = failed_images(KIND=1)
  if (size(sfi) > 0) error stop "failed_images result shall be empty array"
  if (allocated(sfi)) error stop "failed_images result shall not be allocated"

  sfi = failed_images(KIND=8)
  if (size(sfi) > 0) error stop "failed_images result shall be empty array"
! The implicit type conversion in the assignment above allocates an array. 
!  if (allocated(sfi)) error stop "failed_images result shall not be allocated"

end program test_failed_images_1

! { dg-final { scan-tree-dump-times "_gfortran_caf_failed_images \\\(&D\\\.\[0-9\]+, 0B, 0B\\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_failed_images \\\(&D\\\.\[0-9\]+, 0B, D\\\.\[0-9\]+\\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_failed_images \\\(&D\\\.\[0-9\]+, 0B, D\\\.\[0-9\]+\\\);" 1 "original" } }
