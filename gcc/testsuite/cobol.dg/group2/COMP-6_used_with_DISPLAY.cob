       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/COMP-6_used_with_DISPLAY.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-99          PIC 99   USAGE COMP-6.
       01 X-999         PIC 999  USAGE COMP-6.
       PROCEDURE        DIVISION.
           MOVE    0 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE   99 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE    0 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE  123 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           STOP RUN.

