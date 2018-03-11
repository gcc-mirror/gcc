! { dg-do  run }
program main
  implicit none
  integer, parameter :: a(0,3) = 0
  integer, parameter :: b(3,0) = -42
  integer, parameter, dimension(3) :: a1 = minval(a,dim=1)
  integer, parameter, dimension(0) :: a2 = minval(a,dim=2)
  integer, parameter, dimension(0) :: b1 = minval(b,dim=1)
  integer, parameter, dimension(3) :: b2 = minval(b,dim=2)
  logical, parameter :: c(0,3) = .false.
  logical, parameter :: d(3,0) = .false.
  logical, parameter, dimension(3) :: tr = all(c,dim=1)
  logical, parameter, dimension(3) :: fa = any(c,dim=1)
  integer, parameter, dimension(3) :: ze = count(d,dim=2)
  integer, parameter, dimension(3) :: ze2 = iany(b,dim=2)
  integer, parameter, dimension(3) :: ze3 = iparity(a,dim=1)
  real, parameter, dimension(0,3) :: r = 1.0
  real, parameter, dimension(3) :: n2 = norm2(r,dim=1)
  integer, parameter, dimension(3) :: one = product(b,dim=2)
  integer, parameter, dimension(3) :: ze4 = sum(a,dim=1)
  if (any(a1 /= huge(0))) stop 1
  if (any(b2 /= huge(b2))) stop 2
  if (any(.not.tr)) stop 3
  if (any(fa)) stop 3
  if (any(ze /= 0)) stop 4
  if (any(ze2 /= 0)) stop 5
  if (any(ze3 /= 0)) stop 6
  if (any(n2 /= 0.0)) stop 7
  if (any(one /= 1)) stop 8
  if (any(ze4 /= 0)) stop 9
end program main
