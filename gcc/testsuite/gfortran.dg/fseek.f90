! { dg-do run }

PROGRAM test_fseek
  INTEGER, PARAMETER :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2, fd=10
  INTEGER :: ierr = 0
  INTEGER :: newline_length

  ! We first need to determine if a newline is one or two characters
  open (911,status="scratch")
  write(911,"()")
  newline_length = ftell(911)
  close (911)
  if (newline_length < 1 .or. newline_length > 2) STOP 1

  open(fd, status="scratch")
  ! expected position: one leading blank + 10 + newline
  WRITE(fd, *) "1234567890"
  IF (FTELL(fd) /= 11 + newline_length) STOP 2

  ! move backward from current position
  CALL FSEEK(fd, -11 - newline_length, SEEK_CUR, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 0) STOP 3

  ! move to negative position (error)
  CALL FSEEK(fd, -1, SEEK_SET, ierr)
  IF (ierr == 0 .OR. FTELL(fd) /= 0) STOP 4

  ! move forward from end (11 + 10 + newline)
  CALL FSEEK(fd, 10, SEEK_END, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 21 + newline_length) STOP 5

  ! set position (0)
  CALL FSEEK(fd, 0, SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 0) STOP 6

  ! move forward from current position
  CALL FSEEK(fd, 5, SEEK_CUR, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 5) STOP 7

  CALL FSEEK(fd, HUGE(0_1), SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= HUGE(0_1)) STOP 8

  CALL FSEEK(fd, HUGE(0_2), SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= HUGE(0_2)) STOP 9

  CALL FSEEK(fd, HUGE(0_4), SEEK_SET, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= HUGE(0_4)) STOP 10
  
  CALL FSEEK(fd, -HUGE(0_4), SEEK_CUR, ierr)
  IF (ierr /= 0 .OR. FTELL(fd) /= 0) STOP 11
END PROGRAM

