! { dg-do run }
! 
! PR fortran/43551
!
! Writes a 672000 byte file with buffering. The writing failed because
! of a missing lseek.

implicit none
integer, parameter :: size = 2800 ! << needs to be large enough
real(8) :: vec1(size,30), dummy(size)
integer i

CALL RANDOM_NUMBER(vec1)

open(99, file='test.dat', form='unformatted', access='direct', recl=size*8)
do i = 1, 10
  write(99,rec=i) vec1(:,i)
  write(99,rec=i+10) vec1(:,i+10)
  write(99,rec=i+20) vec1(:,i+20) ! << rec = 30 was written to rec = 21
end do

do i = 1, 10
  read(99,rec=i) dummy
  if (any (dummy /= vec1(:,i))) call abort()
  read(99,rec=i+10) dummy
  if (any (dummy /= vec1(:,i+10))) call abort()
  read(99,rec=i+20) dummy
  if (any (dummy /= vec1(:,i+20))) call abort() ! << aborted here for rec = 21
end do

close(99, status='delete')
end

