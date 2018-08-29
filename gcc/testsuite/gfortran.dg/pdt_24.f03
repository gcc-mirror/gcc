! { dg-do compile }
!
! Tests the fixes for PR82866.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module s
   type t(*, a, :) ! { dg-error "A parameter name is required" }
     integer, len :: a
   end type
end
