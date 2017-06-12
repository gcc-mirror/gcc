! { dg-do run }
! { dg-options "-fdump-tree-original -fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program test_stopped_images_1
  implicit none

  integer :: me,np,stat
  character(len=1) :: c
  integer, allocatable :: si(:)
  integer(kind=1), allocatable :: ssi(:)

  si = stopped_images()
  if (size(si) > 0) error stop "stopped_images result shall be empty array at 1"
  if (allocated(si)) error stop "stopped_images result shall not be allocated at 1"

  ssi = stopped_images(KIND=1)
  if (size(ssi) > 0) error stop "stopped_images result shall be empty array at 2"
  if (allocated(ssi)) error stop "stopped_images result shall not be allocated at 2"

  ssi = stopped_images(KIND=8)
  if (size(ssi) > 0) error stop "stopped_images result shall be empty array at 3"
! The implicit type conversion in the assignment above allocates an array. 
!  if (allocated(ssi)) error stop "stopped_images result shall not be allocated at 3"
  
end program test_stopped_images_1

! { dg-final { scan-tree-dump-times "_gfortran_caf_stopped_images \\\(&D\\\.\[0-9\]+, 0B, 0B\\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_stopped_images \\\(&D\\\.\[0-9\]+, 0B, D\\\.\[0-9\]+\\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_stopped_images \\\(&D\\\.\[0-9\]+, 0B, D\\\.\[0-9\]+\\\);" 1 "original" } }
