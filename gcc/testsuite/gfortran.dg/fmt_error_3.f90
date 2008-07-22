! { dg-do compile }

! PR fortran/29835
! Check for improved format error messages with correct locus and more detailed
! "unexpected element" messages.

SUBROUTINE format_labels
  IMPLICIT NONE

1 FORMAT (A, &
          A, &
          Q, & ! { dg-error "Unexpected element 'Q'" }
          A)

2 FORMAT (A, &
          I, & ! { dg-error "Nonnegative width" }
          A)

END SUBROUTINE format_labels

SUBROUTINE format_strings
  IMPLICIT NONE
  CHARACTER(len=32), PARAMETER :: str = "hello"
  INTEGER :: x

  PRINT '(A, Q, A)', & ! { dg-error "Unexpected element 'Q'" }
        str, str, str ! { dg-bogus "Unexpected element" }

  PRINT '(A, ' // & ! { dg-error "Nonnegative width" }
        ' I, ' // &
        ' A)', str, str, str ! { dg-bogus "Nonnegative width" }

  READ '(Q)', & ! { dg-error "Unexpected element 'Q'" }
       x ! { dg-bogus "Unexpected element" }

END SUBROUTINE format_strings
