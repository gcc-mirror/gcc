! { dg-do run }

PROGRAM test_fseek
  INTEGER, PARAMETER :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2, fd=10
  INTEGER :: ierr = 0

  ! expected position: 12, one leading blank + 10 + newline
  WRITE(fd, *) "1234567890"
  IF (FTELL(fd) /= 12) CALL abort()

  ! move backward from current position
  CALL FSEEK(fd, -12, SEEK_CUR, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 0) CALL abort()

  ! move to negative position (error)
  CALL FSEEK(fd, -1, SEEK_SET, ierr)
  IF (ierr == 0 .OR. FTELL(fd) /= 0) CALL abort()

  ! move forward from end (12 + 10)
  CALL FSEEK(fd, 10, SEEK_END, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 22) CALL abort()

  ! set position (0)
  CALL FSEEK(fd, 0, SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 0) CALL abort()

  ! move forward from current position
  CALL FSEEK(fd, 5, SEEK_CUR, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 5) CALL abort()

  CALL FSEEK(fd, HUGE(0_1), SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= HUGE(0_1)) CALL abort()

  CALL FSEEK(fd, HUGE(0_2), SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= HUGE(0_2)) CALL abort()

  CALL FSEEK(fd, HUGE(0_4), SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= HUGE(0_4)) CALL abort()
  
  CALL FSEEK(fd, -HUGE(0_4), SEEK_CUR, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 0) CALL abort()
END PROGRAM

