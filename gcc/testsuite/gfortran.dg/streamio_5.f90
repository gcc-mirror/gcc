! { dg-do run }
! PR25828 Stream IO test 5, unformatted single byte
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program streamtest5
  implicit none
  character(1)   :: lf = char(10)
  character(1)   :: tchar
  integer        :: i,j,k
   
  open(10, file="teststream", access="stream", form="unformatted")
  
  do i=1,1229
    do j=0,9
      write(10) j
    end do
    write(10) lf
  end do
  
  close(10)
  
  open(10, file="teststream", access="stream", form="unformatted")
  
  do i=1,1229
    do j=0,9
      read(10) k
      if (k.ne.j) call abort()
    end do
    read(10) tchar
    if (tchar.ne.lf) call abort()
  end do
  close(10,status="delete")
end program streamtest5