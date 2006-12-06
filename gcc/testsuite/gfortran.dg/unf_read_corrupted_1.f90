! { dg-do run }
! Test the error message when an unformatted file has become
! corrupted.
program main
  implicit none
  integer :: i1, i2
  integer :: ios
  character(len=50) :: msg

  ! Write out a truncated unformatted sequential file by
  ! using unformatted stream.

  open (10, form="unformatted", access="stream", file="foo.dat", &
  status="unknown")
  write (10) 16, 1
  close (10, status="keep")

  ! Try to read
  open (10, file="foo.dat", form="unformatted", access="sequential")
  i1 = 0
  i2 = 0
  read (10, iostat=ios, iomsg=msg) i1, i2
  if (ios == 0) call abort
  if (i1 /= 1) call abort
  if (msg /= "Unformatted file structure has been corrupted") call abort
  close (10, status="delete")
end program main
