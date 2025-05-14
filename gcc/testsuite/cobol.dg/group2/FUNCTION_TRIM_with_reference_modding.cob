       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_TRIM_with_reference_modding.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   X(12) VALUE " a#b.c%d+e$ ".
       PROCEDURE        DIVISION.
           DISPLAY FUNCTION TRIM ( X ) (2 : 3)
           END-DISPLAY.
           DISPLAY FUNCTION TRIM ( X TRAILING ) (2 : 3)
           END-DISPLAY.
           STOP RUN.

