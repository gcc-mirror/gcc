       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-X           PIC XXXX VALUE "0001".
       01 X-9           PIC 9999 COMP VALUE 1.
       PROCEDURE        DIVISION.
         IF X-X = X-9
            STOP RUN
         END-IF.
         DISPLAY "NG" NO ADVANCING
         END-DISPLAY
         STOP RUN.

