! { dg-do run }
! { dg-options "-funsigned" }
! Test some basic formatted I/O.

program main
  unsigned :: u
  open (10,status="scratch")
  write (10,'(I4)') 1u
  write (10,'(I4)') -1
  rewind 10
  read (10,'(I4)') u
  if (u /= 1u) stop 1
  read (10,'(I4)') u
  if (u /= 4294967295u) stop 2
end program main
