! { dg-do run }
! PR43517 Spurious EOF condition when namelist read follows formatted read
! Test case from the problem reporter - Michael Richmond
program main
  namelist /name/ j
  open (10,status='scratch',form='formatted')
  write(10,'(a)') "999999"
  write(10,'(a)') " $name"
  write(10,'(a)') "  j=73,"
  write(10,'(a)') " /"
  rewind(10)
  i = 54321
  idum = 6789
  read (10,'(2i5,4x)') i, idum ! Trailing 4x was setting EOF condition
  if (i /= 99999 .and. idum /= 9) call abort
  j = 12345
  read (10,name) ! EOF condition tripped here.
  if (j /= 73) call abort
end program main

