! { dg-do run }
! { dg-options "-std=gnu" }
character(25) :: sround, ssign, sasynchronous, sdecimal, sencoding
integer :: vsize, vid
logical :: vpending

open(10, file='mydata_f2003_inquire_1', asynchronous="yes", blank="null", &
& decimal="comma", encoding="utf-8", sign="plus")

write (10,*, asynchronous="yes", id=vid) 'asdf'
wait (10)

inquire(unit=10, round=sround, sign=ssign, size=vsize, id=vid, &
& pending=vpending, asynchronous=sasynchronous, decimal=sdecimal, &
& encoding=sencoding)
if (ssign.ne."PLUS") STOP 1
if (sasynchronous.ne."YES") STOP 2
if (sdecimal.ne."COMMA") STOP 3
if (sencoding.ne."UTF-8") STOP 4
if (vpending) STOP 5

close(10, status="delete")
end
