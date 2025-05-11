       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(6) VALUE "123   ".
       PROCEDURE        DIVISION.
           IF X(1:3) NUMERIC
              STOP RUN
           END-IF.
           DISPLAY "NG" NO ADVANCING
           END-DISPLAY.
           STOP RUN.

