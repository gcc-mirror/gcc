! Program to test generic interfaces.
program gen_interf
   implicit none
   interface gen
      function ifn (a)
         integer :: a, ifn
      end function
   end interface
   interface gsub
      subroutine igsub (a)
         integer a
      end subroutine
   end interface
         
   integer i

   call gsub (i)
   i = gen(i)
end program
