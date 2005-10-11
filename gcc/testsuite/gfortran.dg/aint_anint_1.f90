program aint_anint_1
    
  implicit none

  real(4) :: r = 42.7, r1, r2
  real(8) :: s = 42.7D0, s1, s2

  r1 = aint(r)
  r2 = aint(r,kind=8)
  if (abs(r1 - r2) > 0.1) call abort()

  r1 = anint(r)
  r2 = anint(r,kind=8)
  if (abs(r1 - r2) > 0.1) call abort()

  s1 = aint(s)
  s2 = aint(s, kind=4)
  if (abs(s1 - s2) > 0.1) call abort()

  s1 = anint(s)
  s2 = anint(s, kind=4)
  if (abs(s1 - s2) > 0.1) call abort()


end program aint_anint_1

