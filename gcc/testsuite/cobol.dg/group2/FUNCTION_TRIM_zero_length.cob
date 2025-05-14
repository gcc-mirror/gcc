       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_TRIM_zero_length.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  A2   PIC   X(2) VALUE "  ".
       01  A3   PIC   X(3) VALUE "   ".
       01  X   PIC   X(4) VALUE "NOOK".
       PROCEDURE        DIVISION.
           MOVE FUNCTION TRIM ( A2 ) TO X.
           DISPLAY ">" X "<"
           END-DISPLAY.
           DISPLAY ">" FUNCTION TRIM ( A3 ) "<"
           END-DISPLAY.
           STOP RUN.

