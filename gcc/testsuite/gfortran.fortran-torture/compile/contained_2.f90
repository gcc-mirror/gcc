! Arrays declared in parent but used in the child.
program error
   implicit none
   integer, dimension (10) :: a
contains
   subroutine test()
      implicit none
      a(1) = 0
   end subroutine
end program

