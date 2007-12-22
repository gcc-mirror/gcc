! { dg-do run }
! PR34540 cshift, eoshift, kind=1 and kind=2 arguments...
program main
  integer(kind=1) :: d1
  integer(kind=2) :: d2
  integer(kind=4) :: d4
  integer(kind=8) :: d8
  integer(kind=1), dimension(2) :: s1
  integer(kind=2), dimension(2) :: s2
  integer(kind=4), dimension(2) :: s4
  integer(kind=8), dimension(2) :: s8
  real, dimension(2,2) :: r, r1, r2
  data r /1.0, 2.0, 3.0, 4.0/
  data r1 /2.0, 0.0, 4.0, 0.0/
  data r2 /2.0, 1.0, 4.0, 3.0/
  s1 = (/1, 1/)
  s2 = (/1, 1/)
  s4 = (/1, 1/)
  s8 = (/1, 1/)
  d1 = 1
  d2 = 1
  d4 = 1
  d8 = 1
  if (any(eoshift(r,shift=s1,dim=d1) /= r1)) call abort
  if (any(eoshift(r,shift=s2,dim=d2) /= r1)) call abort
  if (any(eoshift(r,shift=s4,dim=d4) /= r1)) call abort
  if (any(eoshift(r,shift=s8,dim=d8) /= r1)) call abort
  if (any(cshift(r,shift=s1,dim=d1) /= r2)) call abort
  if (any(cshift(r,shift=s2,dim=d2) /= r2)) call abort
  if (any(cshift(r,shift=s4,dim=d4) /= r2)) call abort
  if (any(cshift(r,shift=s8,dim=d8) /= r2)) call abort
end program main
