! { dg-do run }
!
! Tests the fix for PR82622 comment #1, where the declaration of
! 'x' choked during initialization. Once fixed, it was found that
! IO was not working correctly for PDT array components.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   character(120) :: buffer
   integer :: i(3)
   type t(a)
      integer, len :: a
      integer :: z = 4
   end type
   type t2(b)
      integer, len :: b
      type(t(1)) :: r(b)
   end type
   type(t2(3)) :: x
   write (buffer,*) x
   read (buffer, *) i
   if (any (i .ne. [4,4,4])) stop 1
   x%r = [t(1)(3),t(1)(2),t(1)(1)]
   write (buffer,*) x
   read (buffer, *) i
   if (any (i .ne. [3,2,1])) stop 2
end
