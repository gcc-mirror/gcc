! { dg-do compile }
! PR fortran/68224
! Original code contribute by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
! 
program p
   integer, parameter :: a(null()) = [1, 2]   ! { dg-error "scalar INTEGER expression" }
   integer, parameter :: b(null():*) = [1, 2]   ! { dg-error "scalar INTEGER expression" }
   integer, parameter :: c(1:null()) = [1, 2]   ! { dg-error "scalar INTEGER expression" }
end program p
