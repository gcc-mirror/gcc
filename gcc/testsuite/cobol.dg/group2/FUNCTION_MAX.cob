       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_MAX.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           DISPLAY FUNCTION MAX ( 3 -14 0 8 -3 )
           END-DISPLAY.
           STOP RUN.

