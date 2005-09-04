! { dg-do run }
! PR 23321 : Running off the end of a file was not detected with direct I/O.
program main
  implicit none
  integer(kind=1) :: a, b
  integer :: ios, i

  a = 42
  open (unit=10,status="scratch",recl=1,access="direct")
  write(10,rec=1) a

  read (10,rec=2, iostat=ios) b
  if (ios == 0) call abort

  read (10, rec=82641, iostat=ios) b      ! This used to cause a segfault
  if (ios == 0) call abort

  read(10, rec=1, iostat=ios) b
  if (ios /= 0) call abort 
  if (a /= b) call abort

end program main
