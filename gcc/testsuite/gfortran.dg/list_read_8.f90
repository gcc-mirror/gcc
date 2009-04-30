! { dg-do run }
! PR34676 IO error delayed
! Test case from PR modified by <jvdelisle@gcc.gnu.org>
implicit none
integer::i,badness
character::c
open(unit=10,status="scratch")
write(10,'(a)') '1'
write(10,'(a)') '2'
write(10,'(a)') '3'
rewind(10)
do i=1,10
  read(10,*,iostat=badness)
  if (badness/=0) exit
enddo
if (i /= 4) call abort
end
