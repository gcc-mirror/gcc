! REPEAT intrinsic, test for PR 31304
! We check that REPEAT accepts all kind arguments for NCOPIES
!
! { dg-do run }
program test
  implicit none

  integer(kind=1) i1
  integer(kind=2) i2
  integer(kind=4) i4
  integer(kind=4) i8
  real(kind=8) r
  character(len=2) s1, s2

  i1 = 1 ; i2 = 1 ; i4 = 1 ; i8 = 1
  r = 1
  s1 = '42'
  r = nearest(r,r)

  s2 = repeat(s1,i1)
  if (s2 /= s1) STOP 1
  s2 = repeat(s1,i2)
  if (s2 /= s1) STOP 2
  s2 = repeat(s1,i4)
  if (s2 /= s1) STOP 3
  s2 = repeat(s1,i8)
  if (s2 /= s1) STOP 4

end program test
