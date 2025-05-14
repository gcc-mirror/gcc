       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X1  PIC   X(14) VALUE " -% 9876.1234 ".
       01  X2  PIC   X(20) VALUE " % 19,876.1234 DB".
       01  N   PIC   s9(5)v9(5).
       PROCEDURE        DIVISION.
           MOVE FUNCTION NUMVAL-C ( X1 , "%" ) TO N
           IF N NOT = -9876.1234
              DISPLAY N
              END-DISPLAY
           END-IF
           MOVE FUNCTION NUMVAL-C ( X2 , "%" ) TO N
           IF N NOT = -19876.1234
              DISPLAY N
              END-DISPLAY
           END-IF
           STOP RUN.

