! { dg-require-effective-target vect_double }

PROGRAM test
  REAL(8) :: f,dist(2)
  dist = [1.0_8, 0.5_8]
  if( f(1.0_8, dist) /= MINVAL(dist)) then
    call abort ()
  endif
END PROGRAM test

FUNCTION f( x, dist ) RESULT(s)
  REAL(8) :: dist(2), x, s
  s = MINVAL(dist)
  IF( x < 0 ) s = -s
END FUNCTION f

