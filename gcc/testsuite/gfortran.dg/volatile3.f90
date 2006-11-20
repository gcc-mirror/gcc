! { dg-do compile }
! { dg-shouldfail "Invalid use of VOLATILE" }
! Test whether volatile statements and attributes are
! properly error checked.
! PR fortran/29601
program volatile_test
  implicit none
  real, external,  volatile :: foo ! { dg-error "VOLATILE attribute conflicts with EXTERNAL attribute" }
  real, intrinsic, volatile :: sin ! { dg-error "VOLATILE attribute conflicts with INTRINSIC attribute" }
  real, parameter, volatile :: r = 5.5 ! { dg-error "PARAMETER attribute conflicts with VOLATILE attribute" }
  real :: l,m
  real,volatile :: n
  real, volatile,volatile :: r = 3. ! { dg-error "Duplicate VOLATILE attribute" }
  volatile :: l,n ! { dg-error "Duplicate VOLATILE attribute" }
  volatile ! { dg-error "Syntax error in VOLATILE statement" }
  volatile :: volatile_test ! { dg-error "PROGRAM attribute conflicts with VOLATILE attribute" }
  l = 4.0
  m = 3.0
contains
  subroutine foo(a) ! { dg-error "has no IMPLICIT type" } ! due to error below
    integer, intent(in), volatile :: a ! { dg-error "VOLATILE attribute conflicts with INTENT\\(IN\\)" }
  end subroutine
end program volatile_test
