! { dg-do "run" }
! Transformational intrinsic MATMUL as initialization expression.

  REAL, PARAMETER :: PI = 3.141592654, theta = PI/6.0

  REAL, PARAMETER :: unity(2,2) = RESHAPE([1.0, 0.0, 0.0, 1.0], [2, 2])
  REAL, PARAMETER :: m1(2,2)    = RESHAPE([COS(theta), SIN(theta), -SIN(theta), COS(theta)], [2, 2])
  REAL, PARAMETER :: m2(2,2)    = RESHAPE([COS(theta), -SIN(theta), SIN(theta), COS(theta)], [2, 2])
  REAL, PARAMETER :: m(2,2)     = MATMUL(m1, m2)

  IF (ANY(ABS(m - unity) > EPSILON(0.0))) CALL abort()
END
