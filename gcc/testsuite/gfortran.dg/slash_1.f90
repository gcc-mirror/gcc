! PR libfortran/22170
! { dg-do run }
  integer i
  open (10,status='scratch')
  write (10,'(A,2/,A)') '12', '17'
  rewind (10)
  read (10,'(I2)') i
  if (i /= 12) call abort
  read (10,'(I2)') i
  if (i /= 0) call abort
  read (10,'(I2)') i
  if (i /= 17) call abort
  end
