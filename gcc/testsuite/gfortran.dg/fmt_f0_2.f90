! { dg-do run }
! PR77393
program testbigf0 ! Can enormous numbers be printed with F0.0 format?
  implicit none
  character(10000) :: str
  write(str, "(f0.0)") -huge(1.0) 
  if (len(trim(str)).lt.41) error stop "FAILED AT LINE 7"
  write(str, "(f0.0)") -huge(1.0_8)
  if (len(trim(str)).lt.311) error stop "FAILED AT LINE 9"
  write(str, "(f0.0)") -huge(1.0_10)
  if (len(trim(str)).lt.4935) error stop "FAILED AT LINE 11"
  write(str, "(f0.10)") -huge(1.0_16)
  if (len(trim(str)).lt.4945) error stop "FAILED AT LINE 13"
end program testbigf0
  
