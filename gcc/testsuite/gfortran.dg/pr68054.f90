! { dg-do compile }
! PR fortran/68054
! Original code contributed by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
!
!program p
   real, protected :: x   ! { dg-error "only allowed in specification" }
end
