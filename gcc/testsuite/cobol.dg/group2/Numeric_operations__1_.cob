       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC S9V9.
       01  Y            PIC S9V9 COMP-3.
       PROCEDURE        DIVISION.
           MOVE -0.1  TO X.
           ADD 1      TO X.
           IF X NOT = 0.9
              DISPLAY X
              END-DISPLAY
           END-IF.
           MOVE  0.1  TO X.
           SUBTRACT 1 FROM X.
           IF X NOT = -0.9
              DISPLAY X
              END-DISPLAY
           END-IF.
           MOVE -0.1 TO Y.
           ADD 1     TO Y.
           IF Y NOT = 0.9
              DISPLAY Y
              END-DISPLAY
           END-IF.
           MOVE  0.1  TO Y.
           SUBTRACT 1 FROM Y.
           IF Y NOT = -0.9
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

