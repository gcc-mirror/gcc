! { dg-do run }
  integer,parameter :: n = 10000
  real(8) array(10000)

  array(:) = 0
  open (10, status='scratch')
  write (10,*) array
  close (10)
end
