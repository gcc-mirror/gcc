! { dg-do run }
! { dg-options "-std=gnu" }
! PR45710 Adjust format/padding for WRITE of NAMELIST group to internal file
program oneline
real :: a=1,b=2,c=3,d=4
namelist /nl1/ a,b,c
parameter(ilines=5)
character(len=80) :: out(ilines)

! fill array out with @
do i=1,len(out)
   out(:)(i:i)='@'
enddo

write(out,nl1)
if (out(1).ne."&NL1") call abort
if (out(2).ne." A=  1.00000000    ,") call abort
if (out(3).ne." B=  2.00000000    ,") call abort
if (out(4).ne." C=  3.00000000    ,") call abort
if (out(5).ne." /") call abort

end program oneline
