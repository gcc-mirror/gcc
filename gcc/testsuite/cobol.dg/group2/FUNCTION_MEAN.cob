       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_MEAN.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 result        PIC S999V999.
       PROCEDURE        DIVISION.
           COMPUTE result = FUNCTION MEAN ( 3 -14 0 8 -3 )
           DISPLAY result
           END-DISPLAY.
           STOP RUN.

