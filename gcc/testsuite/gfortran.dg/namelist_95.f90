! { dg-do run }
! PR83191 Writing a namelist with repeated complex 

program test

implicit none

integer, parameter :: UNIT = 1
character(len=8), parameter :: FILE = "namelist"

complex, dimension(3) :: a = (/ (0.0, 0.0), (0.0, 0.0), (3.0, 4.0) /)

namelist /complex_namelist/ a

open(UNIT, file=FILE)
write(UNIT, nml=complex_namelist)
close(UNIT)

open(UNIT, file=FILE)
read(UNIT, nml=complex_namelist)
close(UNIT, status="delete")
if (any(a.ne.(/ (0.0, 0.0), (0.0, 0.0), (3.0, 4.0) /))) STOP 1
end program test
