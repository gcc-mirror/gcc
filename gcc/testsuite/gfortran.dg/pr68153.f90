! { dg-do compile }
! PR fortran/68153
! Original code contribute by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
!
program foo
   integer, parameter :: a(2) = [2, -2]
   integer, parameter :: b(2,2) = reshape([1, 2, 3, 4], a) ! { dg-error "cannot be negative" }
end program foo
