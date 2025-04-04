       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  TEST-FLD     PIC S9(04)V9(08).
       PROCEDURE        DIVISION.
           MOVE FUNCTION COMBINED-DATETIME ( 987, 345.6 )
             TO TEST-FLD.
           IF TEST-FLD NOT = 987.003456
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

