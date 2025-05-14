       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_MIN.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           DISPLAY FUNCTION MIN ( 3 -14 0 8 -3 )
           END-DISPLAY.
           STOP RUN.

