! { dg-do run }
! PR70684 incorrect reading of values from file on Windows
program test
implicit none
integer,parameter :: isize=12
integer,parameter :: funit=12
integer :: i
character(1), parameter :: cr=char(13)
double precision, dimension(isize) :: a, res
res= (/ 1.0000000000000000, 2.0000000000000000, 3.0000000000000000, &
        4.0000000000000000, 5.0000000000000000, 6.0000000000000000, &
        7.0000000000000000, 8.0000000000000000, 9.0000000000000000, &
        10.000000000000000, 11.000000000000000, 12.000000000000000 /)
do i=1,isize
 a(i)=dble(i)
enddo
open(funit,status="scratch")
write(funit,'(1x,6(f25.20,'',''),a)') (a(i),i=1,6), cr
write(funit,'(1x,6(f25.20,'',''),a)') (a(i),i=7,12), cr
rewind(funit)
a=0d0
read(funit,*) (a(i),i=1,isize)
close(funit)
if (any(a /= res)) call abort
end
