       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC X(10) VALUE "A#B.C%D+E$".
       01  TEST-FLD     PIC X(03).
       PROCEDURE        DIVISION.
           MOVE FUNCTION LOWER-CASE ( X ) (1 : 3)
             TO TEST-FLD
           IF TEST-FLD NOT = 'a#b'
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

