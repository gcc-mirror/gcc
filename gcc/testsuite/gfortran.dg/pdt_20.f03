! { dg-do run }
!
! Tests the fix for PR82622.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t(a)
      integer, len :: a
   end type
   type t2(b)
      integer, len :: b
      type(t(1)) :: r(b)
   end type
   type(t2(:)), allocatable :: x
   allocate (t2(3) :: x)            ! Used to segfault in trans-array.c.
   if (x%b .ne. 3) STOP 1
   if (x%b .ne. size (x%r, 1)) STOP 2
   if (any (x%r%a .ne. 1)) STOP 3
end
