! { dg-do run }
!
! Test the fix for PR88117.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   character(:), pointer :: z(:)
   allocate (z, source  = ['abcd', 'bcde'])
   z = (z) ! gimplifier choked here.
   if (any (z .ne. ['abcd', 'bcde'])) stop 1
   deallocate (z)
end
