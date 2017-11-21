! { dg-do compile }
!
! Test the fix for PR78686, which used to ICE.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
program p
   type t
      character :: c(1) = [t()] ! { dg-error "is being used before it is defined" }
   end type
end
