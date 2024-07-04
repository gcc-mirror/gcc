! { dg-do compile }
! Test the fix for PR103471 in which, rather than giving a "no IMPLICIT type"
! message, gfortran took to ICEing. The fuzzy symbol check for 'kk' demonstrates
! that the error is being detected at the right place.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   implicit none
   integer, parameter :: x(4) = [1,2,3,4]
   real, external :: y
   integer :: kk
   print *, [real(y(l))] ! { dg-error "has no IMPLICIT type" }
   print *, [real(x(k))] ! { dg-error "has no IMPLICIT type; did you mean .kk.\\?" }
! This silently suppresses the error in the previous line. With the line before
! commented out, the error occurs in trans-decl.cc.
!   print *, [real(y(k))]
end
