! { dg-do run }

! PR119502, negative unit numbers are not allowed without using NEWUNIT

program foo
  integer :: iun = -1
  integer :: ios
  open (iun, iostat=ios)
  if (ios == 0) stop 1
  write(iun,*, iostat=ios) "This is a test."
  if (ios == 0) stop 2
  close (iun, iostat=ios)
  if (ios == 0) stop 3
end

