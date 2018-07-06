! { dg-do compile }
! PR fortran/83742
! Contributed by Gerhard Steinmetz <gscfq at t-online dot de>
program p
   real, target :: a
   real, pointer, contiguous :: b => a ! { dg-error "has the CONTIGUOUS attribute" }
end
