       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  TEST-FLD     PIC S9(09)V9(02).
       PROCEDURE        DIVISION.
           MOVE FUNCTION DATE-TO-YYYYMMDD ( 981002, -10, 1994 )
             TO TEST-FLD.
           IF TEST-FLD NOT = 018981002
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

