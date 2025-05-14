       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION ASIN ( -0.2345 ) TO Y.
           IF Y NOT = -0.236704194313346815870178746883458
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

