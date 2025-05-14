       *> { dg-do run }
       *> { dg-output-file "group2/PACKED-DECIMAL_used_with_MOVE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-99          PIC 99   USAGE PACKED-DECIMAL.
       01 X-S99         PIC S99  USAGE PACKED-DECIMAL.
       01 X-999         PIC 999  USAGE PACKED-DECIMAL.
       01 X-S999        PIC S999 USAGE PACKED-DECIMAL.
       01 C-P1234       PIC 9999  VALUE 1234.
       01 C-N1234       PIC S9999 VALUE -1234.
       PROCEDURE        DIVISION.
           MOVE C-P1234 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE C-P1234 TO X-S99.
           DISPLAY X-S99
           END-DISPLAY.
           MOVE C-P1234 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE C-P1234 TO X-S999.
           DISPLAY X-S999
           END-DISPLAY.
           MOVE C-N1234 TO X-99.
           DISPLAY X-99
           END-DISPLAY.
           MOVE C-N1234 TO X-S99.
           DISPLAY X-S99
           END-DISPLAY.
           MOVE C-N1234 TO X-999.
           DISPLAY X-999
           END-DISPLAY.
           MOVE C-N1234 TO X-S999.
           DISPLAY X-S999
           END-DISPLAY.
           STOP RUN.

