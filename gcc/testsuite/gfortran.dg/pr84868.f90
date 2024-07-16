! { dg-do run }
!
! Test the fix for PR84868. Module 'orig' and the call to 'f_orig' is the
! original bug. The rest tests variants and the fix for a gimplifier ICE.
!
! Subroutine 'h' and calls to it were introduced to check the corrections
! needed to fix additional problems, noted in the review of the patch by
! Harald Anlauf
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module orig
   character(:), allocatable :: c
   integer :: ans1(3,3), ans2(3), ans3(2)
contains
   function f_orig(n) result(z)
      character(2), parameter :: c(3) = ['x1', 'y ', 'z2']
      integer, intent(in) :: n
      character(len_trim(c(n))) :: z
      z = c(n)
   end
   function h(n) result(z)
     integer,  intent(in) :: n
     character(2), parameter :: c(3,3) = &
           reshape (['ab','c ','de','f ','gh','i ','jk','l ','mn'],[3,3])
     character(4), parameter :: chr(3) = ['ab  ','  cd','e f ']
     character(len_trim(c(n,n)))  :: z
     z = c(n,n)
! Make sure that full arrays are correctly scalarized both having been previously
! used with an array reference and not previously referenced.
     ans1 = len_trim (c)
     ans2 = len_trim (chr)
! Finally check a slightly more complicated array reference
     ans3 = len_trim (c(1:n+1:2,n-1))
   end
end module orig

module m
   character(:), allocatable :: c
contains
   function f(n, c) result(z)
      character (2) :: c(:)
      integer, intent(in) :: n
      character(len_trim(c(n))) :: z
      z = c(n)
   end
   subroutine foo (pc)
     character(2) :: pc(:)
     if (any ([(len (f(i, pc)), i = 1,3)] .ne. [2,1,2])) stop 1
   end
end
program p
   use m
   use orig
   character (2) :: pc(3) = ['x1', 'y ', 'z2']
   integer :: i

   if (any ([(len (f_orig(i)), i = 1,3)] .ne. [2,1,2])) stop 2 ! ICE

   call foo (pc)
   if (any ([(len (g(i, pc)), i = 1,3)] .ne. [2,1,2])) stop 3
   if (any ([(bar1(i), i = 1,3)] .ne. [2,1,2])) stop 4
   if (any ([(bar2(i), i = 1,3)] .ne. [2,1,2])) stop 5

   if (h(2) .ne. 'gh') stop 6
   if (any (ans1 .ne. reshape ([2,1,2,1,2,1,2,1,2],[3,3]))) stop 7
   if (any (ans2 .ne. [2,4,3])) stop 8
   if (any (ans3 .ne. [2,2])) stop 9
contains
   function g(n, c) result(z)
      character (2) :: c(:)
      integer, intent(in) :: n
      character(len_trim(c(n))) :: z
      z = c(n)
   end
   integer function bar1 (i)
     integer :: i
     bar1 = len (f(i, pc))  ! ICE in is_gimple_min_invariant
   end
   integer function bar2 (i)
     integer :: i
     bar2 = len (g(i, pc))
   end
end
