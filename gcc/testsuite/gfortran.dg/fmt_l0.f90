! { dg-do run }
! { dg-options "-std=gnu -pedantic" }
! Test the GNU extension of a L format descriptor without width
! PR libfortran/54679
program main
  implicit none
  character(len=20) :: str
  character(len=60) :: format2 = "(2(1x,l0,1x))"
  write(str,format2)
end program main
! { dg-output "At line 9 of file.*" }
! { dg-output "Fortran runtime warning: Zero width after L descriptor(\n|\r\n|\r)" }
