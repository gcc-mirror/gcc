! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
program fail_statement
  implicit none

  integer :: me,np,stat

  me = this_image()
  np = num_images()
  stat = 0

  if(me == 1) fail image

  sync all(stat=stat)

  if(stat /= 0) write(*,*) 'Image failed during sync'

end program fail_statement

! { dg-final { scan-tree-dump-times "_gfortran_caf_fail_image \\\(\\\);" 1 "original" } }
