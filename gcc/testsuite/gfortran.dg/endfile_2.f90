! { dg-do run }
! pr18778 abort on endfile without opening unit 
      program test
      implicit none
      integer i
      endfile(8)
      rewind(8)
      read(8,end=0023)i
      call abort ! should never get here
      stop
 0023 continue
      close(8,status='delete')
      end
