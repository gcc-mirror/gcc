! { dg-do run }
!
! Tests the fixes for PR82719 and PR82720.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   character(120) :: buffer
   character(3) :: chr
   integer :: i
   type t(a)
      integer, len :: a
      character(len=a) :: c
   end type
   type(t(:)), allocatable :: x
   allocate (t(2) :: x)

   x = t(2,'ab')
   write (buffer, *) x%c ! Tests the fix for PR82720
   read (buffer, *) chr
   if (trim (chr) .ne. 'ab') call abort

   x = t(3,'xyz')
   if (len (x%c) .ne. 3) call abort
   write (buffer, *) x   ! Tests the fix for PR82719
   read (buffer, *) i, chr
   if (i .ne. 3) call abort
   if (chr .ne. 'xyz') call abort

   buffer = " 3  lmn"
   read (buffer, *) x   ! Some thought will be needed for PDT reads.
   if (x%c .ne. 'lmn') call abort
end
