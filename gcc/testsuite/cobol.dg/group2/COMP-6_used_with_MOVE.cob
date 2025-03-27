       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/COMP-6_used_with_MOVE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-99          PIC 99   USAGE COMP-6.
       01 X-999         PIC 999  USAGE COMP-6.
       01 B-99          USAGE BINARY-LONG.
       01 B-999         USAGE BINARY-LONG.
       PROCEDURE        DIVISION.
           MOVE    0 TO B-99.
           MOVE B-99 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE   99 TO B-99.
           MOVE B-99 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE    0  TO B-999.
           MOVE B-999 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE  123  TO B-999.
           MOVE B-999 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE B-999 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           STOP RUN.

