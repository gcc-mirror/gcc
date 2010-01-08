! { dg-do compile }
! Check for conflicts
! PR fortran/29657

function f1() ! { dg-error "PROCEDURE attribute conflicts with SAVE attribute" }
  implicit none
  real, save :: f1
  f1 = 1.0
end function f1

function f2() ! { dg-error "PROCEDURE attribute conflicts with SAVE attribute" }
  implicit none
  real :: f2
  save f2
  f2 = 1.0
end function f2

subroutine f3()
  implicit none
  dimension f3(3) ! { dg-error "SUBROUTINE attribute conflicts with DIMENSION attribute" }
end subroutine f3

subroutine f4(b)
  implicit none
  real :: b
  entry b ! { dg-error "DUMMY attribute conflicts with ENTRY attribute" }
end subroutine f4

function f5(a)
  implicit none
  real :: a,f5
  entry a ! { dg-error "DUMMY attribute conflicts with ENTRY attribute" }
  f5 = 3.4
end function f5

subroutine f6(cos)
  implicit none
  real :: cos
  intrinsic cos ! { dg-error "DUMMY attribute conflicts with INTRINSIC attribute" }
end subroutine f6

subroutine f7(sin)
  implicit none
  real :: sin
  external sin
end subroutine f7

program test
  implicit none
  dimension test(3) ! { dg-error "PROGRAM attribute conflicts with DIMENSION attribute" }
end program test
