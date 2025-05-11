       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC 9(2) VALUE 0.
       01  Y            PIC 9(2) VALUE 0.
       PROCEDURE        DIVISION.
           COMPUTE X = 100
           END-COMPUTE.
           COMPUTE Y = 99
           END-COMPUTE.
           IF Y NOT = 99
              DISPLAY Y NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

