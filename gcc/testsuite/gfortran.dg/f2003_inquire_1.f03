! { dg-do run }
! { dg-options "-std=gnu" }
character(25) :: sround, ssign, sasynchronous, sdecimal, sencoding
integer :: vsize, vid
logical :: vpending

open(10, file='mydata_f2003_inquire_1', asynchronous="yes", blank="null", &
& decimal="comma", encoding="utf-8", sign="plus")

inquire(unit=10, round=sround, sign=ssign, size=vsize, id=vid, &
& pending=vpending, asynchronous=sasynchronous, decimal=sdecimal, &
& encoding=sencoding)

if (ssign.ne."PLUS") call abort
if (sasynchronous.ne."YES") call abort
if (sdecimal.ne."COMMA") call abort
if (sencoding.ne."UTF-8") call abort
if (vpending) call abort

close(10, status="delete")
end
