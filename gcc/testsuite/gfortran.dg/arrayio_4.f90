! { dg-do run }
! PR 21875 : Test formatted input/output to/from character arrays.
! This test checks the error checking for non-contiguous character
! arrays which are not allowed by standard.  Error 13 is
! ERROR_ARRAY_STRIDE in libgfortran.h
program arrayio_4
  implicit none
  integer        :: ierr
  character(12)  :: r(2,3,4) = '0123456789AB'
   
  write(r(::2,:,::1),'(i5)', iostat=ierr) 1,2,3,4,5
  if (ierr.ne.13) call abort()

  write(r(:,:,::2),'(i5)', iostat=ierr) 1,2,3,4,5
  if (ierr.ne.13) call abort()

  write(r(::1,::2,::1),'(i5)', iostat=ierr) 1,2,3,4,5
  if (ierr.ne.13) call abort()

  write(r(::1,::1,::1),'(i5)', iostat=ierr) 1,2,3,4,5
  if (ierr.ne.0) call abort()
end program arrayio_4

