! { dg-final { cleanup-modules "gfcbug45" } }
! { dg-do run }
! Tests the fix for PR29912, in which the call to JETTER
! would cause a segfault beause a temporary was not being written.
!
! COntributed by Philip Mason  <pmason@ricardo.com>
!
 program testat
 character(len=4)   :: ctemp(2)
 character(len=512) :: temper(2)
 !
 !------------------------
 !'This was OK.'
 !------------------------
 temper(1) = 'doncaster'
 temper(2) = 'uxbridge'
 ctemp     = temper
 if (any (ctemp /= ["donc", "uxbr"])) call abort ()
 !
 !------------------------
 !'This went a bit wrong.'
 !------------------------
 ctemp = jetter(1,2)
 if (any (ctemp /= ["donc", "uxbr"])) call abort ()

 contains
   function jetter(id1,id2)
   character(len=512) :: jetter(id1:id2)
   jetter(id1) = 'doncaster'
   jetter(id2) = 'uxbridge'
   end function jetter
 end program testat

