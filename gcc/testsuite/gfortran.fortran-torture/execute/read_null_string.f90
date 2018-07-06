! pr 16080, segfault on reading an empty string 
      implicit none 
      integer t 
      character*20   temp_name 
      character*2 quotes
      open(unit=7,status='SCRATCH')
      quotes = '""""'  ! "" in the file
      write(7,*)1
      write(7,'(A)')quotes
      temp_name = 'hello'  ! make sure the read overwrites it
      rewind(7)
      read(7, *) t 
      read(7, *) temp_name 
      if (temp_name.ne.'') STOP 1
      end
