! { dg-do run }
! PR 23598 - The iostat variable wasn't reset if the previous
! I/O library call had an error.
program main
  implicit none
  integer :: ios, i
  open (10, pad='no', status='scratch')
  write (10, '(A)') '1','1'
  rewind (10)
  read (10,'(I2)',iostat=ios) i
  ios = -4321
  read (10, '(I1)', iostat=ios) i
  if (ios /= 0) call abort
end program main
