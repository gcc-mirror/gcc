! { dg-do compile }
! { dg-options "-fimplicit-none" }
! PR fortran/69603 - segfault with -fimplicit-none and proc_ptr_comp_24.f90
! Based on reduced testcase by Dominique d'Humieres
PROGRAM prog
  implicit none
  TYPE object
     PROCEDURE(), POINTER, NOPASS :: f
  END TYPE object
  TYPE (object) :: o1
  CALL set_func(o1%f)
CONTAINS
  SUBROUTINE set_func(f)
    PROCEDURE(), POINTER :: f
  END SUBROUTINE set_func
END PROGRAM prog
