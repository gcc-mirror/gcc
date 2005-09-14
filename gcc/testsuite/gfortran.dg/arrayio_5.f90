! { dg-do run }
! PR 21875 : Test formatted input/output to/from character arrays.
! This test checks the error checking for end of file condition.
program arrayio_5
  implicit none
  integer        :: i,ierr
  character(12)  :: r(10) = '0123456789AB'

  write(r,'(i12)',iostat=ierr) 1,2,3,4,5,6,7,8,9,10,11
  if (ierr.ne.-1) call abort()
 end program arrayio_5

