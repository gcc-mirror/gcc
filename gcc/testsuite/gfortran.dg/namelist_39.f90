! { dg-do run }
! PR33421 and PR33253 Weird quotation of namelist output of character arrays
! Test case from Toon Moene, adapted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>

program test
implicit none
character(len=45) :: b(3)
namelist /nam/ b
b = 'x'
open(99, status="scratch")
write(99,'(4(a,/),a)') "&NAM", &
      " b(1)=' AAP NOOT MIES WIM ZUS JET',", &
      " b(2)='SURF.PRESSURE',", &
      " b(3)='APEKOOL',", &
      " /"
rewind(99)
read(99,nml=nam)
close(99)

if (b(1).ne." AAP NOOT MIES WIM ZUS JET                   ") call abort
if (b(2).ne."SURF.PRESSURE                                ") call abort
if (b(3).ne."APEKOOL                                      ") call abort

end program test

