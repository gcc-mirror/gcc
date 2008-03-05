! { dg-do run { target fd_truncate } } 
! PR 19872 - closed and re-opened file not overwriten
      implicit none
      integer i(4)
      data i / 4 * 0 /
      open(1,form='FORMATTED',status='UNKNOWN')
      write(1,'("1 2 3 4 5 6 7 8 9")')
      close(1)
      open(1,form='FORMATTED')
      write(1,'("9 8 7 6")')
      close(1)
      open(1,form='FORMATTED')
      read(1,*)i
      if(i(1).ne.9.and.i(2).ne.8.and.i(3).ne.7.and.i(4).ne.9)call abort
      read(1,*,end=200)i
! should only be able to read one line from the file
      call abort
 200  continue
      close(1,STATUS='DELETE')
      end
