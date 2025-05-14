       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_MIDRANGE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 RESULT PIC S999V999.
       PROCEDURE        DIVISION.
           COMPUTE RESULT = FUNCTION MIDRANGE ( 3 -14 0 8 -3 )
           DISPLAY RESULT
           END-DISPLAY.
           STOP RUN.

