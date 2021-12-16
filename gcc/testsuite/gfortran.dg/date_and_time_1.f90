! PR libfortran/98507
! { dg-do run }

program demo_time_and_date
  implicit none
  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer :: val(8)
  integer :: h, m

  call date_and_time(values=val)

  if (val(1) < 2000 .or. val(1) > 2100) stop 1
  if (val(2) < 1 .or. val(2) > 12) stop 2
  if (val(3) < 1 .or. val(3) > 31) stop 3

  ! Maximum offset is 14 hours (UTC+14)
  if (val(4) < -14*60 .or. val(4) > 14*60) stop 4

  if (val(5) < 0 .or. val(5) > 23) stop 5
  if (val(6) < 0 .or. val(6) > 59) stop 6
  if (val(7) < 0 .or. val(7) > 60) stop 7
  if (val(8) < 0 .or. val(8) > 999) stop 8

  call date_and_time(zone=zone)
  if (len(zone) /= 0) then
    ! If ZONE is present, it should present the same information as
    ! given in VALUES(4)
    if (len(zone) /= 5) stop 9
    read(zone(1:3),*) h
    read(zone(4:5),*) m
    if (val(4) /= 60*h+m) stop 10
  endif
end
