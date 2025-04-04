       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y            PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION COS ( -0.2345 ) TO Y.
           IF Y NOT = 0.972630641256258184713416962414561
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

