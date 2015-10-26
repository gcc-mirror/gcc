! { dg-do run }
! PR25828 Stream IO test 4, Tests string read and writes, single byte.
! Verifies buffering is working correctly and position="append"
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program streamtest
  implicit none
  character(1)   :: lf = char(10)
  character(1)   :: tchar
  integer        :: i,j,k
  integer, parameter :: lines = 5231
   
  open(10, file="teststream_streamio_4", access="stream", form="formatted")
  
  do i=1,lines
    do j=0,9
      write(10,"(i5)") j
    end do
  end do
  
  close(10)
  
  open(10, file="teststream_streamio_4", access="stream",&
  &form="formatted", position="append")
  do i=1,lines
    do j=0,9
      write(10,"(i5)") j
    end do
  end do
  rewind(10)
  do i=1,lines
    do j=0,9
      read(10,"(i5)") k
      if (k.ne.j) call abort()
    end do
  end do

  close(10,status="delete")
end program streamtest
