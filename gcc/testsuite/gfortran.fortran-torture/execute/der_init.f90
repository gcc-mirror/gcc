! Program to test derived type initializers and constructors
program der_init
   implicit none
   type t
      integer :: i
      integer :: j = 4
   end type
   integer :: m, n

   ! Explicit initializer
   type (t) :: var = t(1, 2)
   ! Type (default) initializer
   type (t) :: var2
   ! Initialization of arrays
   type (t), dimension(2) :: var3
   type (t), dimension(2) :: var4 = (/t(7, 9), t(8, 6)/)

   if (var%i .ne. 1 .or. var%j .ne. 2) STOP 1
   if (var2%j .ne. 4) STOP 2
   var2 = t(6, 5)
   if (var2%i .ne. 6 .or. var2%j .ne. 5) STOP 3

   if ((var3(1)%j .ne. 4) .or. (var3(2)%j .ne. 4)) STOP 4
   if ((var4(1)%i .ne. 7) .or. (var4(2)%i .ne. 8) &
       .or. (var4(1)%j .ne. 9) .or. (var4(2)%j .ne. 6)) STOP 5

   ! Non-constant constructor
   n = 1
   m = 5
   var2 = t(n, n + m)
   if (var2%i .ne. 1 .or. var2%j .ne. 6) STOP 6
end program
