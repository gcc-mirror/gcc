# 1 "debug_2.F"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "debug_2.F"
# 3 "debug_2.inc1" 1
# 4 "debug_2.inc2" 1
! The above lines must be present as is.
! PR fortran/34084
! { dg-do compile }
! { dg-options "-g" }
      subroutine foo
      end subroutine foo
# 4 "debug_2.inc1" 2
# 2 "debug_2.F" 2
      program bar
      end program bar
