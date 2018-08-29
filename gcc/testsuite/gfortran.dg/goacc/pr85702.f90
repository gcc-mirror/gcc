! PR fortran/85702
! { dg-do compile }

subroutine s
  !$acc wait(*) ! { dg-error "Invalid argument to ..ACC WAIT" }
end
