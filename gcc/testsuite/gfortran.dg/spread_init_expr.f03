! { dg-do "run" }

  INTEGER, PARAMETER :: n = 5
  INTEGER, PARAMETER :: a1(n) = SPREAD(1, 1, n)
  INTEGER, PARAMETER :: a2(n, 3) = SPREAD([1,2,3], DIM=1, NCOPIES=n)
  INTEGER, PARAMETER :: a3(3, n) = SPREAD([1,2,3], DIM=2, NCOPIES=n)

  IF (ANY(a1 /= [ 1, 1, 1, 1, 1 ])) CALL abort()

  IF (ANY(a2(:, 1) /= 1)) CALL abort()
  IF (ANY(a2(:, 2) /= 2)) CALL abort()
  IF (ANY(a2(:, 3) /= 3)) CALL abort()

  IF (ANY(a3(1, :) /= 1)) CALL abort()
  IF (ANY(a3(2, :) /= 2)) CALL abort()
  IF (ANY(a3(3, :) /= 3)) CALL abort()
END
