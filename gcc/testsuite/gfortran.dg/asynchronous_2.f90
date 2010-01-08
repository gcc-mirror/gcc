! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR/fortran 25829
!
! Check parsing ASYNCHRONOUS
!
function func2() result(res)
  asynchronous res ! { dg-error "Fortran 2003: ASYNCHRONOUS" }
end function func2
