! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/122206
!
! Verify that procedure interfaces are "stable"

module test_example
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none

  abstract interface
     function simple_interface(iarg1, arg2) bind(c) result(res)
       import c_double, c_int
       integer(c_int), value, intent(in) :: iarg1
       real(c_double), value, intent(in) :: arg2
       real(c_double) :: res
     end function simple_interface
  end interface

  procedure(simple_interface), bind(c,name="simple_function") :: simple_function

  interface
     function other_interface(iarg1, arg2) result(res)
       import c_double, c_int
       integer(c_int), value, intent(in) :: iarg1
       real(c_double), value, intent(in) :: arg2
       real(c_double) :: res
     end function other_interface
  end interface

  procedure(other_interface) :: other_function

contains
  subroutine test_example_interface
    implicit none
    integer(c_int) :: iarg1 = 2
    real(c_double) :: arg2  = 10.
    real(c_double) :: val1, val2

    val1 = simple_function(iarg1, arg2)
    val2 = simple_function(iarg1, arg2)
    if (val1 /= val2) stop 1

    val1 = other_function(iarg1, arg2)
    val2 = other_function(iarg1, arg2)
    if (val1 /= val2) stop 2

  end subroutine test_example_interface
end module test_example

! { dg-final { scan-tree-dump-times "simple_function \\(iarg1, arg2\\);" 2 "original"} }
! { dg-final { scan-tree-dump-times "other_function \\(iarg1, arg2\\);" 2 "original"} }
