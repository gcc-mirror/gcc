! { dg-do run }
! Handle eor and eof conditions with missing eor in file.
! Test case modified from case presented by Ian Harvey on clf.
program eieio_stat
  use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
  implicit none
  integer, parameter :: unit=10
  integer :: ios1, ios2, ios3
  character(25) :: buffer
  character(100) :: themessage
  !****
  open(10,file="eieio", form="unformatted", access="stream", status="replace")
  write(10) "Line-1" // char(10)
  write(10) "Line-2"
  close(10)

  open(10,file="eieio")
  
  buffer = 'abcdefg'
  read (unit,"(a)",advance="no",iostat=ios1, pad="yes") buffer
  if (ios1 /= iostat_eor .and. buffer /= "Line-1") STOP 1

  buffer = '<'
  read (unit,"(a)",advance="no",iostat=ios2,pad="yes") buffer
  if (ios2 /= iostat_eor .and. buffer /= "Line-2") STOP 2
  
  buffer = '5678'
  read (unit,"(a)",advance="no",iostat=ios3, iomsg=themessage) buffer
  if (ios3 /= iostat_end .and. buffer /= "5678") STOP 3

  rewind(10)

  buffer = "abcdefg"
  read (unit,"(a)",advance="no",iostat=ios1, pad="no") buffer
  if (ios1 /= iostat_eor .and. buffer /= "abcdefg") STOP 4

  buffer = '<'
  read (unit,"(a)",advance="no",iostat=ios2,pad="no") buffer
  if (ios2 /= iostat_eor .and. buffer /= "<") STOP 5

  buffer = '1234'
  read (unit,"(a)",advance="no",iostat=ios3, iomsg=themessage) buffer
  if (ios3 <= 0 .and. buffer /= "1234") STOP 6
  
  close(unit, status="delete")
end program eieio_stat 
