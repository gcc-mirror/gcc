! { dg-do compile }
! PR fortran/68055
! Original code contributed by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
! 
   integer*3 c    ! { dg-error "not supported at" }
   real*9 x       ! { dg-error "not supported at" }
   logical*11 a   ! { dg-error "not supported at" }
   complex*42 z   ! { dg-error "not supported at" }
   c = 1
   x = 1
   call foo(a)
end
