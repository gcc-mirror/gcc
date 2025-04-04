       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   S9(4)V9(4) VALUE -1.5.
       01  TEST-FLD     PIC S9(04)V9(02).
       PROCEDURE        DIVISION.
           MOVE FUNCTION INTEGER-PART ( X )
             TO TEST-FLD.
           IF TEST-FLD NOT = -1
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

