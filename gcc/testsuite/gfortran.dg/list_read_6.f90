! { dg-do run { target fd_truncate } }
! PR30435 Slash at end of input not recognized according to standard.
! Test case from PR by Steve Kargl.

program t
  integer a, b, c, d
  ! This worked as expected
  open(unit=10, file='tmp.dat')
  write(10,*) '1 2 3 / 4'
  rewind(10)
  a = -1; b = -1; c = -1; d = -1;
  read(10,*) a,b,c,d
  if (d.ne.-1) STOP 1
  
  ! This worked as expected
  rewind(10)
  write(10,*) '1 2 3 /'
  rewind(10)
  a = -2; b = -2; c = -2; d = -2;
  read(10,*) a,b,c,d
  if (d.ne.-2) STOP 2

  ! This worked as expected.
  rewind(10)
  write(10,*) '1 2'
  write(10,*) '3 /'
  rewind(10)
  a = -3; b = -3; c = -3; d = -3;
  read(10,*) a,b,c,d
  if (d.ne.-3) STOP 3

  ! This failed before the patch.
  rewind(10)
  write(10,*) '1 2 3'
  write(10,*) '/'
  rewind(10)
  a = -4; b = -4; c = -4; d = -4;
  read(10,*) a,b,c,d
  if (d.ne.-4) STOP 4

  close(unit=10, status='delete')
end program t
