       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.

       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  str          PIC X(11).

       PROCEDURE        DIVISION.
           MOVE FUNCTION FORMATTED-TIME ("hh:mm:ss,ss", 45296) TO str
           IF str <> "12:34:56,00"
               DISPLAY "Test 1 failed: " str END-DISPLAY
           END-IF

           STOP RUN.

