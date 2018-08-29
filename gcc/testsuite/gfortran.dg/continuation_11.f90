! { dg-do run }
! { dg-options "-Wall -pedantic" }
! Before a bogus warning was printed
!
! PR fortran/39811
!
implicit none
character(len=70) :: str
write(str,'(a)') 'Print rather a lot of ampersands &&&&&
   &&&&&
   &&&&&'
if (len(trim(str)) /= 44 &
    .or. str /= 'Print rather a lot of ampersands &&&&&&&&&&&') &
    STOP 1
end
