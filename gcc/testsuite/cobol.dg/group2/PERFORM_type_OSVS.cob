       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  MYOCC        PIC 9(8) COMP VALUE 0.
       PROCEDURE        DIVISION.
       ASTART SECTION.
       A01.
           PERFORM BTEST.
           IF MYOCC NOT = 2
              DISPLAY MYOCC
              END-DISPLAY
           END-IF.
           STOP RUN.
       BTEST SECTION.
       B01.
           PERFORM B02 VARYING MYOCC FROM 1 BY 1
                   UNTIL MYOCC > 5.
           GO TO B99.
       B02.
           IF MYOCC > 1
              GO TO B99
           END-IF.
       B99.
           EXIT.

