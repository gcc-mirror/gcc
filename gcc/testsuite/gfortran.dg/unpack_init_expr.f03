! { dg-do "run" }
!
! Example from F2003, sec 13.7.125
!
  INTEGER, PARAMETER :: m(3,3) = RESHAPE ([1,0,0,0,1,0,0,0,1], [3,3])
  INTEGER, PARAMETER :: v(3)   = [1,2,3]
  LOGICAL, PARAMETER :: F = .FALSE., T = .TRUE.
  LOGICAL, PARAMETER :: q(3,3) = RESHAPE ([F,T,F,T,F,F,F,F,T], [3,3])

  INTEGER, PARAMETER :: r1(3,3) = UNPACK (V, MASK=Q, FIELD=M)
  INTEGER, PARAMETER :: r2(3,3) = UNPACK (V, MASK=Q, FIELD=0)

  IF (ANY (r1 /= RESHAPE ([1,1,0,2,1,0,0,0,3], [3,3]))) CALL ABORT()
  IF (ANY (r2 /= RESHAPE ([0,1,0,2,0,0,0,0,3], [3,3]))) CALL ABORT()
END
