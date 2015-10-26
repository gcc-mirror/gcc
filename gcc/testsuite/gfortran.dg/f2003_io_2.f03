! { dg-do compile }
! { dg-options "-std=f2003" }
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>

integer :: istat, idvar
character(25) :: msg
real, dimension(10) :: a, b

a = 43.21
open(10, file='mydata_f2003_io_2', asynchronous="yes")
write(10,'(10f8.3)', asynchronous="yes", decimal="comma") a
rewind(10)
read(10,'(10f8.3)', asynchronous="yes", decimal="comma", id=idvar) b
istat = 123456
wait(unit=10, err=25, iostat=istat, iomsg=msg, end=35, id=idvar)

print *, istat

25 continue

35 continue
end
