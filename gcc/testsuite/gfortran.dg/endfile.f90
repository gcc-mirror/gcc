! { dg-do run } 
! pr18364 endfile does not truncate file.
!   write out 20 records
!   rewind
!   read 10 records
!   endfile
!   close file
!   open file
!   detect file has only 10 records  
      implicit none
      integer i,j
      open(unit=10,file='test.dat',access='sequential',status='replace')
      do i=1, 20
        write (10,'(I4)') i
      end do
      rewind(10)
      do i=1,10
        read (10,'(I4)') j
      end do
      endfile(10)
      close(10)
      open(unit=10,file='test.dat',access='sequential',status='old')
      do i=1,20 
        read (10,'(I4)',end=99) j 
      end do
      ! should never get here
      call abort
  99  continue ! end of file
      if (j.ne.10) call abort   
      close(10,status='delete')
      end
