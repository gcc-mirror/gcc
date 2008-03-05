! { dg-do run { target fd_truncate } }
! PR 19451: The test for advance='NO' with eor used to be reversed.
program main
  character*2 c
  open(12, status='SCRATCH')
  write(12, '(A)') '123', '456'
  rewind(12)
  read(12, '(A2)', advance='NO', eor=100) c
100 continue
end program main
