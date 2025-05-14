       *> { dg-do run }
       *> { dg-output-file "group2/PACKED-DECIMAL_used_with_DISPLAY.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-99          PIC 99   USAGE PACKED-DECIMAL.
       01 X-S99         PIC S99  USAGE PACKED-DECIMAL.
       01 X-999         PIC 999  USAGE PACKED-DECIMAL.
       01 X-S999        PIC S999 USAGE PACKED-DECIMAL.
       PROCEDURE        DIVISION.
           MOVE    0 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE   99 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE    0 TO X-S99.
           DISPLAY X-S99
           END-DISPLAY.
           MOVE   -1 TO X-S99.
           DISPLAY X-S99
           END-DISPLAY.
           MOVE    0 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE  123 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE    0 TO X-S999.
           DISPLAY X-S999
           END-DISPLAY.
           MOVE -123 TO X-S999.
           DISPLAY X-S999
           END-DISPLAY.
           STOP RUN.

