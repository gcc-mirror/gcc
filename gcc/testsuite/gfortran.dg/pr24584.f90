! pr24584, segfault on namelist reading an empty string
! Contributed by Jerry DeLisle  <jvdelisle@verizon.net> 
      implicit none 
      character*20   temp
      character(len=10) var
      namelist /input/ var
      var = 'Howdy'
      open(unit=7, status="scratch")
      temp = '   var='''''  ! var='' in the file
      write(7,'(A6)') '&INPUT'
      write(7,'(A10)') temp
      write(7,'(A1)') '/'
      rewind(7)
      read(7,NML=input)
      close(7)
      if (var.ne.'') call abort
      end
