! { dg-do run }
! PR40714 A read hitting EOF should leave the unit structure in a correct state
program test
open(unit=32,status="scratch",access="sequential",form="unformatted")
read(32,end=100)
100 continue
backspace(32)
write (32)
end program test
