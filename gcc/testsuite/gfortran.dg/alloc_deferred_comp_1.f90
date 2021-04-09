! { dg-do run }
!
! Test the fix for PR99125, where the array reference in the print
! statement caused an ICE because the gimplifier complained about '0'
! being used as an lvalue.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      character(:), allocatable :: a(:)
   end type
   type(t) :: x
   character(8) :: c(3) = ['12 45 78','23 56 89','34 67 90']
   x%a = c
   if (any (x%a(2:3) .ne. ['23 56 89','34 67 90'])) stop 1
   if (any (x%a(2:3)(4:5) .ne. ['56','67'])) stop 2 ! Bizarrely this worked.
end
