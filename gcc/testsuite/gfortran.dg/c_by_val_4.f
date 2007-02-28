C { dg-do compile }
C Tests the fix for PR30888, in which the dummy procedure would
C generate an error with the %VAL argument, even though it is
C declared EXTERNAL.
C
C Contributed by Peter W. Draper <p.w.draper@durham.ac.uk>
C
      SUBROUTINE VALTEST( DOIT )
      EXTERNAL DOIT
      INTEGER P
      INTEGER I
      I = 0
      P = 0
      CALL DOIT( %VAL( P ) ) ! { dg-warning "Extension: argument list function" }
      CALL DOIT( I )
      CALL DOIT( %VAL( P ) ) ! { dg-warning "Extension: argument list function" }
      END
