! PR libfortran/23784
! { dg-do run }
  integer i
  close(10, status="whatever", iostat=i)
  if (i == 0) call abort()
  write(17,*) 'foo'
  close(17, status="delete")
  end
