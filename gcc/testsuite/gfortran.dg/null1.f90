! { dg-do compile }
! PR fortran/66045
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
program p
   contains
   integer :: null=null() ! { dg-error "NULL appears on right-hand side" }
end
