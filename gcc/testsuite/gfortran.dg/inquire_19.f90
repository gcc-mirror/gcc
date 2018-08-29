! { dg-do run }
! PR84506  INQUIRE(pos=) always sets pos=0 with -fdefault-integer-8
program TestInquire
   implicit none
   integer(8) :: iUnit
   integer(8) :: iPos
   open(newunit=iunit, file='output.txt', access='stream', status='replace')
   write(iUnit) 'TEXT'
   inquire(iUnit, pos=iPos)
   close(iUnit, status='delete')
   !print *, iPos
   if (iPos.ne.5) stop 1
end program TestInquire
