! { dg-do run }
! PR 30321:  This used to segfault.
program xzero
  implicit none
  integer :: ii(1,0)
  logical :: ll(1,0)
  character (len=80) line
  ll = .true.
  write (unit=line, fmt="(I6)") sum(ii,dim=1)
  if (line /= " ") STOP 1
  write (unit=line, fmt="(I6)") sum(ii,dim=1,mask=ll)
  if (line /= " ") STOP 2
end program xzero
