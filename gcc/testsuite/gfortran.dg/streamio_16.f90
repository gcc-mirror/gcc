! { dg-do run }
! PR38291 Rejects I/O with POS= if FMT=*
character(15) :: sAccess
character(1) :: instr
integer :: mypos, i
mypos = 0
open(50, access="stream", form="formatted")
write(50, *, pos=1) "Just something "
do i=1,17
  read( 50, *,pos=i)
  inquire(50, access=sAccess, pos=mypos)
  if (sAccess.ne."STREAM") STOP 1
  if ((mypos.ne.18).and.(mypos.ne.19)) STOP 2
end do
read (50,*, end=10)
STOP 3
 10 continue
close(50,status="delete")
end
