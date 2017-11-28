! { dg-do run }
! PR 53796 INQUIRE(RECL=...)
program inqrecl
  implicit none
  integer(8) :: r
  integer :: r4
  ! F2018 (N2137) 12.10.2.26: recl for unconnected should be -1
  inquire(10, recl=r)
  if (r /= -1) then
     call abort()
  end if
  
  ! Formatted sequential
  open(10, status="scratch")
  inquire(10, recl=r)
  inquire(10, recl=r4)
  close(10)
  if (r /= huge(0_8) - huge(0_4) - 1) then
     call abort()
  end if
  if (r4 /= huge(0)) then
     call abort()
  end if

  ! Formatted sequential with recl= specifier
  open(10, status="scratch", recl=100)
  inquire(10, recl=r)
  close(10)
  if (r /= 100) then
     call abort()
  end if

  ! Formatted stream
  ! F2018 (N2137) 12.10.2.26: If unit is connected
  ! for stream access, recl should be assigned the value -2.
  open(10, status="scratch", access="stream")
  inquire(10, recl=r)
  close(10)
  if (r /= -2) then
     call abort()
  end if
end program inqrecl
