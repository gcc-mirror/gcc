       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_PRESENT-VALUE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 RESULT PIC 9(5)V9(4).
       PROCEDURE        DIVISION.
           MOVE FUNCTION PRESENT-VALUE ( 3 2 1 ) TO RESULT
           DISPLAY RESULT
           END-DISPLAY.
           STOP RUN.

