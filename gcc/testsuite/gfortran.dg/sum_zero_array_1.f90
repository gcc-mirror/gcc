! { dg-do run }
! PR 30321:  This used to segfault.
program xzero
  implicit none
  integer :: ii(1,0)
  logical :: ll(1,0)
  character (len=80) line
  ll = .true.
  write (unit=line, fmt="(I6)") sum(ii,dim=1)
  if (line /= " ") call abort
  write (unit=line, fmt="(I6)") sum(ii,dim=1,mask=ll)
  if (line /= " ") call abort
end program xzero
