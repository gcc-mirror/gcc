! { dg-do run }
!
! Test the fix for PR81048, where in the second call to 'g2' the
! default initialization was "forgotten". 'g1', 'g1a' and 'g3' check
! that this does not occur for scalars and explicit results.
!
! Contributed by David Smith  <dm577216smith@gmail.com>
!
program test
   type f
       integer :: f = -1
   end type
   type(f) :: a, b(3)
   type(f), allocatable :: ans
   b = g2(a)
   b = g2(a)
   ans = g1(a)
   if (ans%f .ne. -1) call abort
   ans = g1(a)
   if (ans%f .ne. -1) call abort
   ans = g1a(a)
   if (ans%f .ne. -1) call abort
   ans = g1a(a)
   if (ans%f .ne. -1) call abort
   b = g3(a)
   b = g3(a)
contains
   function g3(a) result(res)
      type(f) :: a, res(3)
      do j = 1, 3
         if (res(j)%f == -1) then
             res(j)%f = a%f - 1
         else
             call abort
         endif
      enddo
   end function g3

   function g2(a)
      type(f) :: a, g2(3)
      do j = 1, 3
         if (g2(j)%f == -1) then
             g2(j)%f = a%f - 1
         else
             call abort
         endif
      enddo
   end function g2

   function g1(a)
     type(f) :: g1, a
     if (g1%f .ne. -1 ) call abort
   end function

   function g1a(a) result(res)
     type(f) :: res, a
     if (res%f .ne. -1 ) call abort
   end function
end program test
