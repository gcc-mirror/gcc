! Program to check using parent variables in more than one contained function
program contained_3
   implicit none
   integer var
contains
   subroutine one
      var = 1
   end subroutine
   subroutine two
      var = 2
   end subroutine
end program
