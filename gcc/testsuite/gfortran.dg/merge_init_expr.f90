! { dg-do run }
!
! Check simplification of MERGE.
!

  INTEGER, PARAMETER :: array(3) = [1, 2, 3]
  LOGICAL, PARAMETER :: mask(3)  = [ .TRUE., .FALSE., .TRUE. ]

  INTEGER, PARAMETER :: scalar_1 = MERGE (1, 0, .TRUE.)
  INTEGER, PARAMETER :: scalar_2 = MERGE (0, 1, .FALSE.)

  INTEGER, PARAMETER :: array_1(3) = MERGE (array, 0, .TRUE.)
  INTEGER, PARAMETER :: array_2(3) = MERGE (array, 0, .FALSE.)
  INTEGER, PARAMETER :: array_3(3) = MERGE (0, array, .TRUE.)
  INTEGER, PARAMETER :: array_4(3) = MERGE (0, array, .FALSE.)
  INTEGER, PARAMETER :: array_5(3) = MERGE (1, 0, mask)
  INTEGER, PARAMETER :: array_6(3) = MERGE (array, -array, mask)

  INTEGER, PARAMETER :: array_7(3) = MERGE ([1,2,3], -array, mask)

  IF (scalar_1 /= 1 .OR. scalar_2 /= 1) STOP 1
  IF (.NOT. ALL (array_1 == array)) STOP 2
  IF (.NOT. ALL (array_2 == [0, 0, 0])) STOP 3
  IF (.NOT. ALL (array_3 == [0, 0, 0])) STOP 4
  IF (.NOT. ALL (array_4 == array)) STOP 5
  IF (.NOT. ALL (array_5 == [1, 0, 1])) STOP 6
  IF (.NOT. ALL (array_6 == [1, -2, 3])) STOP 7
END
