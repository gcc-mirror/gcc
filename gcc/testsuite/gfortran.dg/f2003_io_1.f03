! { dg-do run { target fd_truncate } }
! { dg-options "-std=gnu" }
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
real :: a(4), b(4)
real :: c
integer :: istat, j
character(25) :: msg

a = 23.45
b = 0.0
open(10, file='mydata', asynchronous="yes", blank="null")

write(10,'(10f8.3)', asynchronous="yes", decimal="comma", id=j) a
rewind(10)
read(10,'(10f8.3)', asynchronous="yes", decimal="comma", blank="zero") b
if (any(b.ne.23.45)) call abort

c = 3.14
write(msg, *, decimal="comma") c
if (msg(1:7).ne."   3,14") call abort

b = 0.0
rewind(10)
write(10,'(10f8.3)', asynchronous="yes", decimal="point") a
rewind(10)
read(10,'(10f8.3)', asynchronous="yes", decimal="point") b
if (any(b.ne.23.45)) call abort

wait(unit=10, err=25, iostat=istat, iomsg=msg, end=35, id=j)

! do some stuff with a
25 continue

35 continue

close(10, status="delete")
end
