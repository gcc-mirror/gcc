       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
         02 X           PIC X OCCURS 10 INDEXED I.
       PROCEDURE        DIVISION.
           SET I TO ZERO.
           SET X(1) TO I
           IF X(1) NOT = "0"
              DISPLAY X(1) NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

