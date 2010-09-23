! { dg-do run }
! PR45532 gfortran namelist read error.
! Derived from the original test case by David Sagan.
program test
implicit none
type line_struct
  integer :: width = 10
end type
type symbol_struct
  integer :: typee = 313233
end type
type curve_struct
  type (line_struct) line
  type (symbol_struct) symbol
end type
type (curve_struct) curve(10)
namelist / params / curve
!
open (10, status="scratch")
write(10,*) "&params"
write(10,*) " curve(1)%symbol%typee = 1234"
write(10,*) "/"
rewind(10)
read (10, nml = params)
if (curve(1)%symbol%typee /= 1234) call abort
close(10)
end program
