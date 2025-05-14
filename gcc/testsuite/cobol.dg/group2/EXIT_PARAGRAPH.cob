       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 INDVAL        PIC 9(4).
       PROCEDURE        DIVISION.
       A01.
           PERFORM VARYING INDVAL FROM 1 BY 1 UNTIL INDVAL > 10
            IF INDVAL > 2
               EXIT PARAGRAPH
            END-IF
           END-PERFORM.
       A02.
           IF INDVAL NOT = 3
              DISPLAY INDVAL NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

