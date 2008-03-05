! PR libfortran/23784
! { dg-do run { target fd_truncate } }
  integer i
  close(10, status="whatever", iostat=i) ! { dg-warning "STATUS specifier in CLOSE statement.*has invalid value" }
  if (i == 0) call abort()
  write(17,*) 'foo'
  close(17, status="delete")
  end
