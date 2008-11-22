! { dg-do run }
! { dg-options "-fbounds-check" }
program main
  implicit none
  integer, dimension(2,2) :: a4
  integer(kind=1), dimension(2,2) :: a1
  character(len=100) line
  data a4 /1, 2, 3, 4/
  a1 = a4
  write (unit=line,fmt='(4I3)') reshape(a4,(/4/))
  write (unit=line,fmt='(4I3)') reshape(a1,(/4/))
end program main
