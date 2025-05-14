       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_ABS.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   S9(4)V9(4) VALUE -1.2345.
       PROCEDURE        DIVISION.
           COMPUTE X = FUNCTION ABS( X )
           DISPLAY X
           END-DISPLAY.
           STOP RUN.

