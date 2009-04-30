! { dg-do run }
! PR25828 Stream IO test 3, tests read_x and inquire.
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
program streamio_3
  implicit none
  integer         :: i(6),j
  character(10)   :: myaccess
  open(10, access="stream", form="formatted")
  i = (/(j,j=1,6)/)
  write(10,'(3(2x,i4/)/3(3x,i6/))') i
  i = 0
  rewind(10)
  read(10,'(3(2x,i4/)/3(3x,i6/))') i
  if (any(i.ne.(/(j,j=1,6)/))) call abort()
  inquire(unit=10, access=myaccess)
  if (myaccess.ne."STREAM") call abort()
  close(10,status="delete")
end program streamio_3
