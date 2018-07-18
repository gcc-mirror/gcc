! { dg-do run }
! Test the error message when an unformatted file has become
! corrupted.
program main
  implicit none
  integer(kind=4) :: i1, i2
  integer :: ios
  character(len=50) :: msg

  ! Write out a truncated unformatted sequential file by
  ! using unformatted stream.

  open (10, form="unformatted", access="stream", file="foo_unf_read_corrupted_1.dat", &
  status="unknown")
  write (10) 16_4, 1_4
  close (10, status="keep")

  ! Try to read
  open (10, file="foo_unf_read_corrupted_1.dat", form="unformatted", access="sequential")
  i1 = 0
  i2 = 0
  read (10, iostat=ios, iomsg=msg) i1, i2
  if (ios == 0) STOP 1
  if (i1 /= 1) STOP 2
  if (msg /= "Unformatted file structure has been corrupted") STOP 3
  close (10, status="delete")
end program main
