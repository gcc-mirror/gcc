! { dg-do compile }
!
! Test the fix for PR99060 in which the expression caused an ICE after the error.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   real :: a
   print *, a%kind%n ! { dg-error "not an inquiry reference" }
end
