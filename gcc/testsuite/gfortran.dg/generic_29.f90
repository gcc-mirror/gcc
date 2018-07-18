! { dg-do compile }
! PR fortran/66057
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
program p
   type t
      contains
      generic :: ! { dg-error "Malformed GENERIC" }
   end type
end
