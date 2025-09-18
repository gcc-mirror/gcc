! { dg-do compile }
!
! Test the fix for PR82587
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
program p
   type t(a)                   ! { dg-error "does not have a component" }
      integer(kind=t()) :: x   ! { dg-error "Expected initialization expression" }
   end type
end
