! PR fortran/45308
! { dg-do run }
 character(len=36) :: date, time
 date = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
 time = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
 call date_and_time (date, time)
 if (index (date, 'a') /= 0 .or. index (time, 'a') /= 0) &
   STOP 1
end
