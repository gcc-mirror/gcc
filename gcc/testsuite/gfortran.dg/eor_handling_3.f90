! { dg-do run }
! PR 19595:  Handle end-of-record condition with pad=yes (default)
program main
  integer i1, i2
  open(77,status='scratch')
  write (77,'(A)') '123','456'
  rewind(77)
  read(77,'(2I2)',advance='no',eor=100) i1,i2
  STOP 1
100 continue
  if (i1 /= 12 .or. i2 /= 3) STOP 2
  close(77)
end program main
