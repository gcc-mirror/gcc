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
   integer :: i(4)
   type t(a)
      integer, len :: a
   end type
   type t2(b)
      integer, len :: b
      type(t(1)) :: r(b)
   end type
   type(t2(3)) :: x
   write (buffer,*) x
   read (buffer,*) i
   if (any (i .ne. [3,1,1,1])) STOP 1
end
