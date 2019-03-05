! { dg-do run }
module foo_nml
   implicit none
   real :: x = -1
   namelist /foo/ x
end module

program main
   use foo_nml, only: bar => foo, x
   implicit none
   integer fd
   x = 42
   open(newunit=fd, file='tmp.dat', status='replace')
   write(fd,nml=bar)
   close(fd)
   open(newunit=fd, file='tmp.dat', status='old')
   read(fd,nml=bar)
   if (x /= 42) stop 1
   close(fd)
end program
! { dg-final { cleanup-modules "foo_nml" } }
