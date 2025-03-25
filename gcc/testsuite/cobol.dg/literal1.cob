*> { dg-do run }
*> Make sure we properly round to integer when computing the initial
*> binary representation of a literal
IDENTIFICATION          DIVISION.
PROGRAM-ID.             literal1.
DATA                    DIVISION.
WORKING-STORAGE         SECTION.
      77 VAR8 PIC 999V9(8) COMP-5 .
      77 VAR555 PIC 999V99999999 COMP-5 VALUE 555.55555555.
      PROCEDURE               DIVISION.
      MOVE 555.55555555 TO VAR8
      ADD 0.00000001 TO VAR555 GIVING VAR8 ROUNDED
      IF VAR8 NOT EQUAL TO 555.55555556 STOP RUN ERROR 1.
      END PROGRAM             literal1.
