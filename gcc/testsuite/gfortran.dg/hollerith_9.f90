! { dg-do compile }
! PR 91800 - this used to cause an ICE.
module m
   type t(n) ! { dg-error "does not have a component corresponding to parameter" }
      integer, len :: n = 4habcd ! { dg-error "Initialization of structure component with a HOLLERITH constant" }
   end type
end
