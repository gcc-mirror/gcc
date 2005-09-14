! { dg-do run }
! PR 21875 : Test formatted input/output to/from character arrays.
! This test deliberately exceeds the record length in a write and
! verifies the error message.
      program arrayio_3
      implicit none
      integer        :: i(6),j,ierr
      character(12)  :: r(4,2) = '0123456789AB'
 
! Write using a format string that defines a record greater than 
! the length of an element in the character array.

      i = (/(j,j=1,6)/)
      write(r,'(3(2x,i4/)/3(4x,i9/))', iostat=ierr) i
      if (ierr.ne.-2) call abort()
      end program arrayio_3
