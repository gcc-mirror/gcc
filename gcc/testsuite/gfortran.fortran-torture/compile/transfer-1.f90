! Bigendian test posted by Perseus in comp.lang.fortran on 4 July 2005.
   integer(1), parameter :: zero = 0
   LOGICAL, PARAMETER :: bigend = IACHAR(TRANSFER(1,"a")) == zero
   LOGICAL :: bigendian
   call foo ()
contains
   subroutine foo ()
   integer :: chr, ans
   if (bigend) then
     ans = 1
   else
     ans = 0
   end if
   chr = IACHAR(TRANSFER(1,"a"))
   bigendian =  chr == 0_4
   if (bigendian) then
     ans = 1
   else
     ans = 0
   end if
   end subroutine foo
end
