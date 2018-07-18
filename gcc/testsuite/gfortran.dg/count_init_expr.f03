! { dg-do run }

  INTEGER            :: i
  INTEGER, PARAMETER :: m(4,4) = RESHAPE([ (i, i=1, 16) ], [4, 4] )
  INTEGER, PARAMETER :: sevens = COUNT (m == 7)
  INTEGER, PARAMETER :: odd(4) = COUNT (MOD(m, 2) == 1, dim=1)
  INTEGER, PARAMETER :: even = COUNT (MOD(m, 2) == 0)

  IF (sevens /= 1) STOP 1
  IF (ANY(odd /= [ 2,2,2,2 ])) STOP 2
  IF (even /= 8) STOP 3

  ! check the kind parameter
  IF (KIND(COUNT (m == 7, KIND=2)) /= 2) STOP 4
END
