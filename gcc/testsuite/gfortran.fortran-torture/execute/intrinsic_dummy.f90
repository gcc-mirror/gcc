! Program to test passing intrinsic functions as actual arguments for
! dummy procedures.
subroutine test (proc)
   implicit none
   real proc
   real a, b, c

   a = 1.0
   b = sin (a)
   c = proc (a)
   
   if (abs (b - c) .gt. 0.001) call abort
   
end subroutine

program dummy
   implicit none
   external test
   intrinsic sin
   
   call test (sin)
end program

