! { dg-do run }
!
! Test the fix for PR93833, which ICEd as shown.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   character(:), allocatable :: c
   c = "wxyz"
contains
   subroutine s
      associate (y => [c])
         if (any(y /= [c])) stop 1
      end associate
   end
end
