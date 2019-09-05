! { dg-do compile }
!
! Check the fix for PR91589, in which the invalid expression caused an ICE.
! Other statements using this invalid expression cause "Unclassifiable statement at..."
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer :: a
   end type
   type(t) :: x = t(1)
   call sub (x%a%a)   ! { dg-error "Syntax error in argument list" }
end

