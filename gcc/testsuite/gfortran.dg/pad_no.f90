! { dg-do run { target fd_truncate } }
! Test correct operation for pad='no'.
program main
  character(len=1) line(2)
  line = 'x'
  open(77,status='scratch',pad='no')
  write(77,'(A)') 'a','b'
  rewind(77)
  read(77,'(2A)',iostat=i) line(1)
  if (line(1) /= 'a' .or. line(2) /= 'x') call abort
  rewind(77)
  line = 'y'
  read(77,'(2A)',iostat=i,advance='no') line
  if (line(1) /= 'a' .or. line(2) /= 'y') call abort
end program main
