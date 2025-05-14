       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  INDVAL       PIC 9(4).
       PROCEDURE        DIVISION.
           PERFORM VARYING INDVAL FROM 1
            BY 1 UNTIL INDVAL > 2
            CONTINUE
            END-PERFORM
           IF INDVAL NOT = 3
              DISPLAY INDVAL NO ADVANCING
              END-DISPLAY
           END-IF
           .

