! { dg-do run }
!
! Run-time test for EVENT_TYPE
!
use iso_fortran_env, only: event_type
implicit none

type(event_type), save :: var[*]
integer :: count, stat

count = -42
call event_query (var, count)
if (count /= 0) call abort()

stat = 99
event post (var, stat=stat)
if (stat /= 0) call abort()
call event_query(var, count, stat=stat)
if (count /= 1 .or. stat /= 0) call abort()

stat = 99
event post (var[this_image()])
call event_query(var, count)
if (count /= 2) call abort()

stat = 99
event wait (var)
call event_query(var, count)
if (count /= 1) call abort()

stat = 99
event post (var)
call event_query(var, count)
if (count /= 2) call abort()

stat = 99
event post (var)
call event_query(var, count)
if (count /= 3) call abort()

stat = 99
event wait (var, until_count=2)
call event_query(var, count)
if (count /= 1) call abort()

stat = 99
event wait (var, stat=stat, until_count=1)
if (stat /= 0) call abort()
call event_query(event=var, stat=stat, count=count)
if (count /= 0 .or. stat /= 0) call abort()
end
