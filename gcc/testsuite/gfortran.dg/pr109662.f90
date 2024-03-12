! { dg-do run }
! { dg-options "-std=f2003" }
! PR109662 a comma after namelist name accepted on input. 
program testnmlread
  implicit none
  character(16) :: list = '&stuff, n = 759/'
  character(100)::message
  integer       :: n, ioresult
  namelist/stuff/n
  message = ""
  ioresult = 0
  n = 99
  read(list,nml=stuff,iostat=ioresult)
  if (ioresult == 0) STOP 13
end program testnmlread
