! Program to test the stack variable size limit.
program stack
   call sub1
   call sub2 (1)
contains

   ! Local variables larger than 32768 in byte size shall be placed in static
   ! storage area, while others be put on stack by default.
   subroutine sub1
      real a, b(32768/4), c(32768/4+1) 
      integer m, n(1024,4), k(1024,1024)
      a = 10.0
      b = 20.0
      c = 30.0
      m = 10
      n = 20
      k = 30
      if ((a .ne. 10.0).or.(b(1) .ne. 20.0).or.(c(1) .ne. 30.0)) STOP 1
      if ((m .ne. 10).or.(n(256,4) .ne. 20).or.(k(1,1024) .ne. 30)) STOP 2
   end subroutine

   ! Local variables defined in recursive subroutine are always put on stack.
   recursive subroutine sub2 (n)
      real a (32769)
      a (1) = 42
      if (n .ge. 1) call sub2 (n-1)
      if (a(1) .ne. 42) STOP 3
      a (1) = 0
   end subroutine
end
