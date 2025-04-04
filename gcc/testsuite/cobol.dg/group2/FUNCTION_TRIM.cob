       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_TRIM.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   X(12) VALUE " a#b.c%d+e$ ".
       PROCEDURE        DIVISION.
           DISPLAY FUNCTION TRIM ( X )
           END-DISPLAY.
           DISPLAY FUNCTION TRIM ( X TRAILING )
           END-DISPLAY.
           STOP RUN.

