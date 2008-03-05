! { dg-do run { target fd_truncate } } 
! PR31099 Runtime error on legal code using RECL
program test
  integer(kind=4) :: a, b
  a=1
  b=2
  open(10, status="scratch", form="unformatted", recl=8)
  write(10) a,b
  write(10) a,b
  write(10) a,b
  write(10) b, a
  rewind(10)
  write(10) a,b
  write(10) a,b
  write(10) a,b
  write(10) b, a
  b=0
  a=0
  rewind(10)
  read(10) a, b
  read(10) a, b
  read(10) a, b
  read(10) a, b
  if ((a.ne.2).and.( b.ne.1)) call abort()
end program test

