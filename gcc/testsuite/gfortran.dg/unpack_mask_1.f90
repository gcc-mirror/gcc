! { dg-do run }
! PR 32731 - upack lacked conversion for kind=1 and kind=2 mask
program main
  implicit none
  character(len=80) line
  logical(kind=1),dimension(2,2) :: mask1
  logical(kind=1),dimension(2,2) :: mask2
  mask1 = .true.
  mask2 = .true.
  write(unit=line,fmt='(4I4)') unpack((/1,2,3,4/),mask1,0)
  write(unit=line,fmt='(4I4)') unpack((/1,2,3,4/),mask2,0)
end program main
