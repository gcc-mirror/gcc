! { dg-do compile }
! PR fortran/31639 -- ICE on invalid initialization expression

function f()
  integer :: i = irand()     ! { dg-error "not permitted in an initialization expression" }
  f = i
end function
