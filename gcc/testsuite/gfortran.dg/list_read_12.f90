! { dg-do run }
! PR58324 Bogus end of file condition
integer :: i, ios
open(99, access='stream', form='unformatted')
write(99) "5 a"
close(99)

open(99, access='sequential', form='formatted')
read(99, *, iostat=ios) i
close(99, status="delete")
if (ios /= 0) STOP 1
end
