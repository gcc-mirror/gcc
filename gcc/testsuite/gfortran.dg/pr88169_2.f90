! { dg-do run }
module foo_nml
   implicit none
   real :: x = -1
   namelist /foo/ x
end module
!
! Yes, implicit typing of local variable 'x'.
!
program main
   use foo_nml, only: bar => foo
   integer fd
   x = 42
   open(newunit=fd, file='tmp.dat', status='replace')
   write(fd,nml=bar)
   close(fd)
   open(newunit=fd, file='tmp.dat', status='old')
   read(fd,nml=bar)
   close(fd)
   call bah
   if (x /= 42) stop 1
end program

subroutine bah
   use foo_nml
   integer fd
   open(newunit=fd, file='tmp.dat', status='old')
   read(fd,nml=foo)
   if (x /= -1) stop 2
   close(fd, status='delete')
end subroutine bah
