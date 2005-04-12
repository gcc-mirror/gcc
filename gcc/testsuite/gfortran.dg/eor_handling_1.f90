! { dg-do run }
! PR 17992:  Reading an empty file should yield zero with pad='YES'
! (which is the default).
! Test case supplied by milan@cmm.ki.si.
program main
  open(77,status='scratch')
  write(77,'(A)') '',''
  rewind(77)
  i = 42
  j = 42
  read(77,'(/2i2)') i,j
  if (i /= 0 .or. j /= 0) call abort
  close(77)
end program main
