! { dg-do run }
! PR 62768
! Filenames with embedded NULL characters are truncated, make sure
! inquire reports the correct truncated name.
program filename_null
  implicit none
  character(len=15), parameter :: s = "hello" // achar(0) // "world", &
       s2 = "hello"
  character(len=15) :: r
  logical :: l
  open(10, file=s)
  inquire(unit=10, name=r)
  if (r /= s2) STOP 1
  inquire(file=s2, exist=l)
  if (.not. l) STOP 2
  close(10, status="delete")
end program filename_null
