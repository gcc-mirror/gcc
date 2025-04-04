       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_ORD.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 RESULT PIC 999.
       PROCEDURE        DIVISION.
           MOVE FUNCTION ORD ( "k" ) TO RESULT
           DISPLAY RESULT
           END-DISPLAY.
           STOP RUN.

