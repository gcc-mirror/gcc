! Obscure failure that disappeared when the parameter was removed.
! Works OK now.
module mymod
implicit none
contains
   subroutine test(i)
      implicit none
      integer i
   end subroutine
end module mymod

program error
   use mymod
end program

