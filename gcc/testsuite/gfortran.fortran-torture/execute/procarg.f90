! Pogram to test
subroutine myp (a)
   implicit none
   integer a

   if (a .ne. 42) STOP 1
end subroutine

subroutine test2 (p)
   implicit none
   external p

   call p(42)
end subroutine

subroutine test (p)
   implicit none
   external p, test2

   call p(42)
   call test2(p)
end subroutine

program arrayio
   implicit none
   external test, myp

   call test (myp)
end program
