       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_ORD-MIN.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 RESULT PIC 999.
       PROCEDURE        DIVISION.
           MOVE FUNCTION ORD-MIN ( 3 -14 0 8 -3 ) TO RESULT
           DISPLAY RESULT
           END-DISPLAY.
           STOP RUN.

