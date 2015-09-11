!{ dg-do compile }
! PR fortran/66040
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
real function f1(x)
   sequence          ! { dg-error "Unexpected SEQUENCE statement" }
end function f1

real function f2()
   else              ! { dg-error "Unexpected ELSE statement" }
end function f2

real function f3()
   block data        ! { dg-error "Unexpected BLOCK DATA statement" }
end function f3

real function f4()
   program p         ! { dg-error "Unexpected PROGRAM statement" }
end function f4
