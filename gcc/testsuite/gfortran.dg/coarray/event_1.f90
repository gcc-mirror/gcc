! { dg-do run }
!
! Run-time test for EVENT_TYPE
!
use iso_fortran_env, only: event_type
implicit none

type(event_type), save, allocatable, dimension(:) :: events[:]
integer :: count, stat

associate (me => this_image(), np => num_images())
  allocate(events(np)[*])

  associate(var => events(me))
    count = -42
    call event_query (var, count)
    if (count /= 0) STOP 1

    stat = 99
    event post (var, stat=stat)
    if (stat /= 0) STOP 2
    call event_query(var, count, stat=stat)
    if (count /= 1 .or. stat /= 0) STOP 3

    count = 99
    event post (var[this_image()])
    call event_query(var, count)
    if (count /= 2) STOP 4

    count = 99
    event wait (var)
    call event_query(var, count)
    if (count /= 1) STOP 5

    count = 99
    event post (var)
    call event_query(var, count)
    if (count /= 2) STOP 6

    count = 99
    event post (var)
    call event_query(var, count)
    if (count /= 3) STOP 7

    count = 99
    event wait (var, until_count=2)
    call event_query(var, count)
    if (count /= 1) STOP 8
   
    stat = 99
    event wait (var, stat=stat, until_count=1)
    if (stat /= 0) STOP 9
    count = 99
    call event_query(event=var, stat=stat, count=count)
    if (count /= 0 .or. stat /= 0) STOP 10
  end associate
end associate
end
