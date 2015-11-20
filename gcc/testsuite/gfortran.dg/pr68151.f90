! { dg-do compile }
! PR fortran/68151
! Original code contribute by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
!
program p
   integer :: k = 1
   select case (k)
   case (:huge(1._4))   ! { dg-error "Expression in CASE" }
   case (:huge(2._8))   ! { dg-error "Expression in CASE" }
   case ((1.0,2.0))     ! { dg-error "Expression in CASE" }
   end select
end
