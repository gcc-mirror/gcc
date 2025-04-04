       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  str          PIC X(04).
       PROCEDURE        DIVISION.
           MOVE FUNCTION FORMATTED-DATETIME
               ("YYYYMMDDThhmmss", 1, 1) (3:4)
             TO STR
           IF STR NOT = '0101'
              DISPLAY STR
              END-DISPLAY
           END-IF
           STOP RUN.

