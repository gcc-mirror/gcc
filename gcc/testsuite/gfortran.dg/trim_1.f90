! { dg-do run }

! Torture-test TRIM and LEN_TRIM for correctness.


! Given a total string length and a trimmed length, construct an
! appropriate string and check gfortran gets it right.

SUBROUTINE check_trim (full_len, trimmed_len)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: full_len, trimmed_len
  CHARACTER(LEN=full_len) :: string

  string = ""
  IF (trimmed_len > 0) THEN
    string(trimmed_len:trimmed_len) = "x"
  END IF

  IF (LEN (string) /= full_len &
      .OR. LEN_TRIM (string) /= trimmed_len &
      .OR. LEN (TRIM (string)) /= trimmed_len &
      .OR. TRIM (string) /= string (1:trimmed_len)) THEN
    PRINT *, full_len, trimmed_len
    PRINT *, LEN (string), LEN_TRIM (string)
    CALL abort ()
  END IF
END SUBROUTINE check_trim


! The main program, check with various combinations.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: i, j

  DO i = 0, 20
    DO j = 0, i
      CALL check_trim (i, j)
    END DO
  END DO
END PROGRAM main
