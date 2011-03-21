! { dg-do run }
! Transformational intrinsic TRANSPOSE as initialization expression.

  INTEGER, PARAMETER :: n = 10
  INTEGER, PARAMETER :: a(n,1) = RESHAPE([ (i, i = 1, n) ], [n, 1])
  INTEGER, PARAMETER :: b(1,n) = TRANSPOSE(a)
  INTEGER, PARAMETER :: c(n,1) = TRANSPOSE(b)

  IF (ANY(c /= a)) CALL abort()
END
