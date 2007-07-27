! { dg-do compile }
! PR fortran/31580
!
! Testcase contributed by Tobias Burnus <burnus AT gcc DOT gnu DOT org>
!
PROGRAM test
  real :: a,b
  if(a .nonex. b) stop       ! { dg-error "Unknown operator" }
end program
