! { dg-do compile }
! PR68567
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>
!
program p
   integer, parameter :: a(:) = [2, 1] ! { dg-error "cannot be automatic or of deferred shape" }
end
