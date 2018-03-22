! { dg-do run }
! PR 44292 Large RECL=
program large_recl
  implicit none
  integer(kind=8) :: r
  open(10, status="scratch", recl=12345678901_8, form="unformatted", access="direct")
  inquire(10, recl=r)
  close(10, status="delete")
  if (r /= 12345678901_8) then
     STOP 1
  end if
end program large_recl
