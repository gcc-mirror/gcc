! { dg-do run }
!
! Run-time test for EVENT_TYPE
!
use iso_fortran_env, only: event_type
implicit none

type(event_type), save, allocatable :: var(:)[:]
integer :: count, stat

allocate(var(3)[*])

count = -42
call event_query (var(1), count)
if (count /= 0) STOP 1
call event_query (var(1), count)
if (count /= 0) STOP 2
call event_query (var(2), count)
if (count /= 0) STOP 3
call event_query (var(3), count)
if (count /= 0) STOP 4

stat = 99
event post (var(2), stat=stat)
if (stat /= 0) STOP 5
call event_query (var(1), count)
if (count /= 0) STOP 6
call event_query(var(2), count, stat=stat)
if (count /= 1 .or. stat /= 0) STOP 7
call event_query (var(3), count)
if (count /= 0) STOP 8

stat = 99
event post (var(2)[this_image()])
call event_query(var(1), count)
if (count /= 0) STOP 9
call event_query(var(2), count)
if (count /= 2) STOP 10
call event_query(var(2), count)
if (count /= 2) STOP 11
call event_query(var(3), count)
if (count /= 0) STOP 12

stat = 99
event wait (var(2))
call event_query(var(1), count)
if (count /= 0) STOP 13
call event_query(var(2), count)
if (count /= 1) STOP 14
call event_query(var(3), count)
if (count /= 0) STOP 15

stat = 99
event post (var(2))
call event_query(var(1), count)
if (count /= 0) STOP 16
call event_query(var(2), count)
if (count /= 2) STOP 17
call event_query(var(3), count)
if (count /= 0) STOP 18

stat = 99
event post (var(2))
call event_query(var(1), count)
if (count /= 0) STOP 19
call event_query(var(2), count)
if (count /= 3) STOP 20
call event_query(var(3), count)
if (count /= 0) STOP 21

stat = 99
event wait (var(2), until_count=2)
call event_query(var(1), count)
if (count /= 0) STOP 22
call event_query(var(2), count)
if (count /= 1) STOP 23
call event_query(var(3), count)
if (count /= 0) STOP 24

stat = 99
event wait (var(2), stat=stat, until_count=1)
if (stat /= 0) STOP 25
call event_query(event=var(1), stat=stat, count=count)
if (count /= 0 .or. stat /= 0) STOP 26
call event_query(event=var(2), stat=stat, count=count)
if (count /= 0 .or. stat /= 0) STOP 27
call event_query(event=var(3), stat=stat, count=count)
if (count /= 0 .or. stat /= 0) STOP 28
end
