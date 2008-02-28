! { dg-do compile }
! { dg-options "-std=gnu -Wreturn-type" }
! PR fortran/34248
!
! There was an ICE for assumed-length functions
! if RESULT(...) was used and no value assigned
! to the result variable.
!
character(*) FUNCTION test() RESULT(ctab)
  ctab = "Hello"
END function test

FUNCTION test2() RESULT(res)      ! { dg-warning "not set" }
  character(*) :: res
END function test2
