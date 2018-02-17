! { dg-do run }
! pr37083 formatted read of line without trailing new-line fails
real :: a, b, c
open(unit=10,file="atest",access='stream',form='unformatted',&
     & status="replace")
write(10) '1.2'//achar(10)//'2.2'//achar(10)//'3.'
call fputc(10,'3')
close(10, status="keep")
open(unit=10,file="atest",form='formatted',status="old")
read(10,*) a, b, c
if (a.ne.1.2 .or. b.ne.2.2 .or. c.ne.3.3) STOP 1
close(10, status="delete")
end
