program contained
   implicit none
   integer i

   i = 0;
   call testproc (40)
   if (i .ne. 42) call abort
contains
   subroutine testproc (p)
      implicit none
      integer p

      if (p .ne. 40) call abort
      i = p + 2
   end subroutine
end program
